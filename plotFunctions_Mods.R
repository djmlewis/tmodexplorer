
plotSelectedModules <- function(moddata,selmod,t,l,z,medians,grouper,gg){
  plot <-  NULL
  if (!is.null(selmod) && nrow(selmod) > 0 && !is.null(moddata)) {
    
    data2plot <- moddata %>%
      filter(Module %in% unique(selmod$Module), Column %in% unique(selmod$Column))
    data2plot[[grouper]] <- factor(data2plot[[grouper]], levels = unique(data2plot[[grouper]]))
    # we have to reorder as data is arranged alphabetically
    data2plot[[grouper]] <- fct_reorder(data2plot[[grouper]],data2plot$Value,median)
    
    if(gg == FALSE) {
      xmax <- max(data2plot$Value,na.rm = TRUE)
      xmin <- min(data2plot$Value,na.rm = TRUE)

      plot <- plotBaseBoxplot(data2plot[[grouper]],data2plot$Value,NULL,paste0('Selected Modules\n',t),z,l,xmax,xmin)
    } else {

        plot <-  ggplot(
        data = data2plot,
        mapping = aes_string(
          x = grouper,
          y = 'Value',
          colour = grouper,
          fill = grouper
        )
      ) +
        geom_boxplot(alpha = 0.5, outlier.alpha = 1.0,show.legend=l) + coord_flip() +
        ggtitle(paste0('Selected Modules\n',t)) +
        themeBase(FALSE)
  
      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
    }
  }
  return(plot)
}

plotSelectedModulesSeries <- function(alldata,selCol,selmod,t,l,z,boxRibbon,facet,showSE,grouper,xgrid,point,sortCol){
  plot <-  NULL
  data2plot <- NULL
  if (!is.null(alldata)) {
    showNotification("Please wait for plot output…", type = 'message', duration = 3)
    
    # strip out the titles
    mods <- sub(' .*$', '', selmod)

    # honour the selCol order
    origlevels <- unique(sub("_.*","",selCol))
    
    switch (boxRibbon,
      'Boxplot' = {
        dataset <- 'modules'
        yCol <- 'Value'},
      'Lines' = {
        dataset <- 'modulesMeans'
        yCol = 'Mean'}
    )
    
    data2plot <- alldata[[dataset]]%>%
      filter(Module %in% mods, Column %in% selCol)

    if(nrow(data2plot)>0) { # clicking Plot without endtering columns

      if(facet == TRUE) {
        
        data2plot <- data2plot %>%
          separate(Column,into = c('Treatment','Column'),sep = '_', convert = TRUE) %>%
          # preserve the order of entry
          mutate(Treatment = factor(Treatment, levels = origlevels))

        if(boxRibbon == 'Boxplot') {
          data2plot <- data2plot %>% mutate(Column = factor(data2plot$Column, levels = unique(data2plot$Column)))
          }
        data2plot <- data2plot %>%
          arrange(Treatment, Module, Column)
      } else {
        data2plot <- data2plot %>% mutate(Column = factor(Column, levels = unique(selCol)))
        data2plot <- data2plot %>%
          arrange(Module, Column)
      }
      

      plot <-  ggplot(
        mapping = aes_string(
          x = 'Column',
          y = yCol
        )
      ) +
        ggtitle(paste0('Selected Modules\n',t)) +
        themeBase(facet == FALSE)
      
      if(z == TRUE) {
        plot <-  plot + geom_hline(data = data2plot, yintercept = 0.0, linetype = 2, show.legend = FALSE)
      }
      if(xgrid == TRUE && boxRibbon == 'Lines' && facet == TRUE) {
        plot <- plot + geom_vline(data = data2plot, xintercept = unique(data2plot$Column), color = 'grey80', alpha = 0.5, show.legend = FALSE)
      }
      
      
      switch (boxRibbon,
        'Boxplot' = {
          plot <- plot + geom_boxplot(data = data2plot, mapping = aes_string(colour = grouper,fill = grouper), alpha = 0.5, outlier.alpha = 1.0,show.legend=l)
        },
        'Lines' = {
          # ModuleMeans is ordered by Module so we have to recalculate
          if(grouper == 'Title') {
            if(facet == TRUE) {
              data2plot <- data2plot %>%
                group_by(Title,Treatment,Column) %>%
                summarise_if(is.numeric,mean)
            } else {
                data2plot <- data2plot %>%
                  group_by(Title,Column) %>%
                  summarise_if(is.numeric,mean)
            }
          }
          plot <- plot + 
            geom_line(data = data2plot, mapping = aes_string(colour = grouper, group = grouper), size = 1,show.legend=l)
          
          if(point == TRUE) {plot <-  plot + geom_point(data = data2plot,mapping = aes_string(colour = grouper, group = grouper), size = 2 ,show.legend=l)}
          
          if(facet == TRUE) {plot <- plot + scale_x_continuous(breaks = data2plot$Column)}
          if(showSE == TRUE){
            data4SE <- data2plot %>%
              mutate(ymin = Mean-SE, ymax = Mean+SE)
            plot <- plot +
              geom_ribbon(data = data4SE, mapping = aes_string(x = 'Column', ymin = 'ymin', ymax = 'ymax',fill = grouper,group = grouper), alpha = 0.2,show.legend=l)
          }
        }
      )
      
      if(facet == TRUE) {
        plot <- plot + facet_wrap(~Treatment)
      }
      
      #sortCol - VACCINE_DAY must come last or factors go awry
      if(sortCol %in% selCol) {
        plot <- addSortColPlot(sortCol,facet,plot,levels(data2plot$Column))
      }
      
    }
  }
  return(list(plot = plot, table = data2plot))
}
