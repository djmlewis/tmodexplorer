plotSelectedModules <- function(moddata,selmod,t,l,z,medians,grouper){
  plot <-  NULL
  if (!is.null(selmod) && nrow(selmod) > 0 && !is.null(moddata)) {
    data2plot <- moddata %>%
      filter(Module %in% unique(selmod$Module), Column %in% unique(selmod$Column)) %>%
      #arrange in order
      mutate(Module = factor(Module, levels = rev(unique(selmod$Module))))

    plot <-  ggplot(
      data = data2plot,
      mapping = aes_string(
        x = 'Module',
        y = 'Value',
        colour = grouper,
        fill = grouper
      )
    ) +
      geom_boxplot(alpha = 0.2, outlier.alpha = 1.0,show.legend=l) + coord_flip() +
      geom_point(data = selmod, mapping = aes(x = Module, y=Mean),show.legend=FALSE, shape = 4) +
      ggtitle(paste0('Selected Modules\n',t)) +
      themeBase
    
    if(z == TRUE) {
      plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
    }
  }
  return(plot)
}

plotSelectedModulesSeries <- function(alldata,selCol,selmod,t,l,z,boxRibbon,facet,showSE){
  plot <-  NULL
  data2plot <- NULL
  if (!is.null(alldata)) {
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
        if(boxRibbon == 'Boxplot') {data2plot <- data2plot %>% mutate(Column = factor(Column, levels = selCol))}
        data2plot <- data2plot %>%
          arrange(Module, Column)
      }
      
      plot <-  ggplot(
        data = data2plot,
        mapping = aes_string(
          x = 'Column',
          y = yCol,
          colour = 'Module',
          fill = 'Module'
        )
      ) +
        ggtitle(paste0('Selected Modules\n',t)) +
        themeBase
      
      switch (boxRibbon,
        'Boxplot' = {
          plot <- plot + geom_boxplot(alpha = 0.2, outlier.alpha = 1.0,show.legend=l)
        },
        'Ribbon' = {
          plot <- plot + 
            geom_line(mapping = aes_string(group = 'Module'),show.legend=l) +
            scale_x_continuous(breaks = data2plot$Column)
          if(showSE == TRUE){
            data4SE <- data2plot %>%
              mutate(ymin = Mean-SE, ymax = Mean+SE)
            plot <- plot +
              geom_ribbon(data = data4SE, mapping = aes(x = Column, ymin = ymin, ymax = ymax,group = Module), alpha = 0.2,show.legend=l)
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
