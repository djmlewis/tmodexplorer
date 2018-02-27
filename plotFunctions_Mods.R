plotSelectedModules <- function(moddata,selmod,t,l,z,medians,grouper,gg){
  plot <-  NULL
  if (!is.null(selmod) && nrow(selmod) > 0 && !is.null(moddata)) {
    data2plot <- moddata %>%
      filter(Module %in% unique(selmod$Module), Column %in% unique(selmod$Column))
    
    if(gg == FALSE) {
      xmax <- max(data2plot$Value,na.rm = TRUE)
      xmin <- min(data2plot$Value,na.rm = TRUE)
      data2plot[[grouper]] <- gsub(' ','\n', data2plot[[grouper]])
      data2plot[[grouper]] <- factor(data2plot[[grouper]], levels = unique(data2plot[[grouper]]))
      selmod <- selmod %>% select_at(c(grouper,'Mean'))
      selmod[[grouper]] <- as.factor(selmod[[grouper]])
      plot <- plotBaseBoxplot(as.factor(data2plot[[grouper]]),data2plot$Value,NULL,paste0('Selected Modules\n',t),z,l,xmax,xmin)
    } else {

        plot <-  ggplot(
        data = data2plot,
        mapping = aes_string(
          x = grouper, #'Module',
          y = 'Value',
          colour = grouper,
          fill = grouper
        )
      ) +
        geom_boxplot(alpha = 0.2, outlier.alpha = 1.0,show.legend=l) + coord_flip() +
        geom_point(data = selmod, mapping = aes_string(x = grouper, y='Mean'),show.legend=FALSE, shape = 4) +
        ggtitle(paste0('Selected Modules\n',t)) +
        themeBase
  
      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
    }
  }
  return(plot)
}

plotSelectedModulesSeries <- function(alldata,selCol,selmod,t,l,z,boxRibbon,facet,showSE,grouper){
  plot <-  NULL
  data2plot <- NULL
  if (!is.null(alldata)) {
    showNotification("Please wait for plot output…", type = 'message', duration = 3)
    
    # strip out the titles
    mods <- sub(' .*$', '', selmod)

    switch (boxRibbon,
      'Boxplot' = {
        dataset <- 'modules'
        yCol <- 'Value'},
      'Ribbon' = {
        dataset <- 'modulesMeans'
        yCol = 'Mean'}
    )
    
    data2plot <- alldata[[dataset]]%>%
      filter(Module %in% mods, Column %in% selCol)
    
    if(nrow(data2plot)>0) { # clicking Plot without endtering columns

      if(facet == TRUE) {
        data2plot <- data2plot %>%
          separate(Column,into = c('Treatment','Column'),sep = '_', convert = TRUE)
        if(boxRibbon == 'Boxplot') {data2plot <- data2plot %>% mutate(Column = as.factor(Column))}
        data2plot <- data2plot %>%
          arrange(Treatment, Module, Column)
      } else {
        data2plot <- data2plot %>% mutate(Column = factor(Column, levels = unique(selCol)))
        data2plot <- data2plot %>%
          arrange(Module, Column)
      }
      
      plot <-  ggplot(
        data = data2plot,
        mapping = aes_string(
          x = 'Column',
          y = yCol
        )
      ) +
        ggtitle(paste0('Selected Modules\n',t)) +
        themeBase
      
      switch (boxRibbon,
        'Boxplot' = {
          plot <- plot + geom_boxplot(mapping = aes_string(colour = grouper,fill = grouper), alpha = 0.2, outlier.alpha = 1.0,show.legend=l)
        },
        'Ribbon' = {
          plot <- plot + 
            # Ribbon/line aes() does not work with Title which has duplicates, impose Module
            geom_line(mapping = aes(colour = Module, group = Module),show.legend=l)
          if(facet == TRUE) {plot <- plot + scale_x_continuous(breaks = data2plot$Column)}
          if(showSE == TRUE){
            data4SE <- data2plot %>%
              mutate(ymin = Mean-SE, ymax = Mean+SE)
            plot <- plot +
              geom_ribbon(data = data4SE, mapping = aes(x = Column, ymin = ymin, ymax = ymax,fill = Module,group = Module), alpha = 0.2,show.legend=l)
          }
        }
      )

      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
      
      if(facet == TRUE) {
        plot <- plot + facet_wrap(~Treatment)
      }
    }
  }
  return(list(plot = plot, table = data2plot))
}
