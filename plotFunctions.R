themeBase <- theme_bw() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    legend.title = element_text()
  )


plotGenesModules <- function(d,t,l){
  if (!is.null(d) && nrow(d) > 0) {
    ggplot(
      data = d,
      mapping = aes(
        x = Module,
        y = Value,
        colour = Module,
        fill = Module
      )
    ) +
      geom_hline(yintercept = 0.0, linetype = 2) +
      geom_boxplot(alpha = 0.2, outlier.alpha = 1.0,show.legend=l) + coord_flip() +
      ggtitle(paste0('Modules For Selected Genes\n',t)) +
      themeBase
  }
}

plotModuleGenes <- function(d,m,t,l) {
  if (!is.null(d) && nrow(d) > 0) {
    d <- d %>%
      filter(!is.na(Value))
    ggplot(
      data = d,
      mapping = aes(
        x = Gene,
        colour = Gene,
        fill = Gene
      )
    ) +
      geom_text(mapping = aes(label = Selected), y = -Inf, hjust = 0, show.legend=FALSE) +
      geom_hline(yintercept = 0.0, linetype = 2) +
      geom_boxplot(mapping = aes(y = Value),alpha = 0.2, outlier.alpha = 1.0, show.legend=l) +
      coord_flip() +
      ggtitle(paste0('Genes for module ',m,'\n',t)) +
      themeBase
  }
}

plotTopGenesInSeries <- function(data2plot, asGenes, selCols,connectPoints,showlegend,t){
  if(is.null(data2plot)) return(NULL)
  
  if(asGenes){
    plotData <- data2plot
  } else {
    plotData <- data2plot %>%
      mutate(Gene = paste0(Gene,' (',Probe,')')) %>%
      select(-c(Probe))
  }
  plotData <- plotData %>%
    gather(key = 'Column', value = 'Value',-Gene, convert = TRUE, factor_key = FALSE) %>%
    mutate(Column = factor(Column, levels = selCols))
  
  ggplot(
    data = plotData,
    mapping = aes(
      x = Column,
      y = Value,
      colour = Gene,
      fill = Gene,
      group = Gene
    )
  ) +
    geom_hline(yintercept = 0.0, linetype = 2) +
    geom_point(show.legend=showlegend) +
    {if(connectPoints){geom_line(show.legend=showlegend)}} +
    ggtitle(paste0('Selected Genes\n',t)) +
    themeBase
}

plotModulesInSeries <- function(d,t,l,r,f){
  p <-  NULL
  if (!is.null(d) && nrow(d) > 0) {
    p <- ggplot(data = d, mapping = aes(x = Column)) +
      geom_hline(yintercept = 0.0, linetype = 2) +
      ggtitle(paste0('Modules For Selected Genes\n',t)) +
      themeBase

    if(r == 'Ribbon'){
      p <- p +
        geom_ribbon(mapping = aes(ymin = SElo, ymax = SEhi, fill = Module, group = Module), alpha = 0.2,show.legend=l) +
        geom_line(aes(y = Value, colour = Module, group = Module),show.legend=l) +
        scale_x_continuous(breaks = unique(d$Column))
    } else {
      p <- p +
        geom_boxplot(mapping = aes(y = Value, colour = Module, fill = Module), alpha = 0.2, outlier.alpha = 1.0,show.legend=l)
    }
    
    if(f == TRUE){
      p <- p +
        facet_wrap(~Treatment)
    }
  }
  return(p)
}


downloadTableCSV <- function(table2save,fname){
  downloadHandler(fname,function(file) {
    write.csv(table2save, file, row.names = FALSE)})
}

downloadPlotPNG <- function(plot2save,fname){
    downloadHandler(fname,function(file) {
    ggsave(file, plot = plot2save, device = 'png', width = 400, height = 300, units = 'mm')
  })
}