themeBase <- theme_bw() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    legend.title = element_text()
  )

plotBaseBoxplot <- function(x,y,s,t,z,l,xmax,xmin){
  
  ncols <- length(levels(x))
  colpal <- rainbow(ncols, alpha = 0.2)
  bordpal <- rainbow(ncols)
  insetv <- -0.16
  legcols <- (ncols %/% 37) + 1
  

  original.parameters<- par( no.readonly = TRUE )
  par(mai = c(0.7, 1.6, 0.8, ifelse(l == TRUE,1.6*legcols,0.8)))
  plot <-  {
    boxplot(y ~ x, col = colpal,border = bordpal, pars = list(las = 2), horizontal = TRUE, outline = TRUE)
    if(!is.null(s)) text(y = x, x = xmin, labels = s, pos = 2)
    title(t)
    if(z == TRUE) abline(v = 0.0, xpd = FALSE, col = "gray60", lty = 'dashed')
    if(l == TRUE) legend(x = xmax+xmax/18, y = ncols+(ncols/18),bty = 'n',ncol = legcols, horiz = FALSE, inset = c(insetv,0), legend = levels(x), fill = colpal, xpd = TRUE)
  }
  par(original.parameters)
}

plotGenesModules <- function(d,t,l,z,gg){
  plot <-  NULL
  if (!is.null(d) && nrow(d) > 0) {
    d <- d %>%
      filter(!is.na(Value)) %>%
      mutate(Module = droplevels(Module))
    xmax <- max(d$Value,na.rm = TRUE)
    xmin <- min(d$Value,na.rm = TRUE)
    
    if(gg == FALSE){
      plot <- plotBaseBoxplot(d$Module,d$Value,NULL,paste0('Modules For Selected Genes\n',t),z,l,xmax,xmin)
    } else {
      plot <-  ggplot(
        data = d,
        mapping = aes(
          x = Module,
          y = Value,
          colour = Module,
          fill = Module
        )
      ) +
        geom_boxplot(alpha = 0.2, outlier.alpha = 1.0,show.legend=l) + coord_flip() +
        ggtitle(paste0('Modules For Selected Genes\n',t)) +
        themeBase
  
      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
    }
  }
  return(plot)
}


plotModuleGenes <- function(d,m,t,l,z,gg) {
  plot <- NULL
  if (!is.null(d) && nrow(d) > 0) {
    d <- d %>%
      filter(!is.na(Value)) %>%
      mutate(Gene = droplevels(Gene))
    
    xmax <- max(d$Value,na.rm = TRUE)
    xmin <- min(d$Value,na.rm = TRUE)
    
    if(gg == TRUE) {
      plot <- ggplot(
        data = d,
        mapping = aes(
          x = Gene,
          colour = Gene,
          fill = Gene
        )
      ) +
      geom_text(mapping = aes(label = Selected, y = xmin), nudge_y = -0.02, hjust = 0, show.legend=FALSE) +
      geom_boxplot(mapping = aes(y = Value),alpha = 0.2, outlier.alpha = 1.0, show.legend=l) +
      coord_flip() +
      ggtitle(paste0('Genes for module ',m,'\n',t)) +
      themeBase
      
      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
    } else {
      plot <- plotBaseBoxplot(d$Gene,d$Value,d$Selected,paste0('Genes for module ',m,'\n',t),z,l,xmax,xmin)
    }
  }
  return(plot)
}

plotTopGenesInSeries <- function(data2plot,
           asGenes,
           connectPoints,
           showlegend,
           t,
           facet,
           showZero,pointsBoxes) {
    if (is.null(data2plot)) return(NULL)
    
    if (asGenes) {
      plotData <- data2plot
    } else {
      # merge gene and probe names if not averaged
      plotData <- data2plot %>%
        mutate(Gene = paste0(Gene, ' (', Probe, ')')) %>%
        select(-c(Probe))
    }
    plotData <- plotData %>%
      mutate(Gene = factor(Gene, levels = unique(Gene)))
    if (facet == FALSE) {
      plotData <- plotData %>%
        mutate(Column = factor(Column, levels = unique(Column)))
    }
    
      plot <-   ggplot(data = plotData) +
      {if(pointsBoxes == 'Boxplot' && facet == TRUE) {geom_boxplot(mapping = aes(x = Column, y = Value, group = Column, colour = Treatment, fill = Treatment), alpha = 0.2, outlier.alpha = 1.0, show.legend = showlegend)}} +
      {if(pointsBoxes == 'Boxplot' && facet == FALSE) {geom_boxplot(mapping = aes(x = Column, y = Value, group = Column), colour = 'black', fill = 'black', alpha = 0.2, outlier.alpha = 1.0, show.legend = FALSE)}} +
      {if(pointsBoxes == 'Points'){geom_point(mapping = aes(x = Column,y = Value,colour = Gene,fill = Gene,group = Gene), show.legend = showlegend)}} +
      {if (pointsBoxes == 'Points' && connectPoints) {geom_line(mapping = aes(x = Column,y = Value,colour = Gene,group = Gene), show.legend = showlegend)}} +
      ggtitle(paste0('Selected Genes\n', t)) +
      themeBase

    if (showZero == TRUE) {
      plot <- plot + geom_hline(yintercept = 0.0, linetype = 2)
    }

    if (facet == TRUE) {
      plot <-  plot +
        scale_x_continuous(breaks = plotData$Column) +
        facet_wrap( ~ Treatment)
    }
    
    return(plot)
  }

plotModulesInSeries <- function(d,t,l,r,f,z,se){
  p <-  NULL
  if (!is.null(d) && nrow(d) > 0) {
    p <- ggplot(data = d, mapping = aes(x = Column)) +
      ggtitle(paste0('Modules For Selected Genes\n',t)) +
      themeBase
    
    if(z == TRUE) {
      p <- p +
        geom_hline(yintercept = 0.0, linetype = 2)
    }
    
    if(r == 'Ribbon'){
      if(length(unique(d$Column)) > 1) {
        p <- p +
          {if(se == TRUE){geom_ribbon(mapping = aes(ymin = SElo, ymax = SEhi, fill = Module, group = Module), alpha = 0.2,show.legend=l)}} +
          geom_line(aes(y = Value, colour = Module, group = Module),show.legend=l)
      } else {# cannot plot lines and ribbons with only 1 point
        p <- p +
          geom_point(aes(y = Value, colour = Module, group = Module),show.legend=l)
      }
      
      # if facet_wrap we split column into real x values. If not we have factors. So only add scale_x_continuous to facet_wrap
      if(f == TRUE) {
        p <- p +scale_x_continuous(breaks = unique(d$Column))
      }
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


downloadGeneList <- function(list,fname){
  fname <- paste0(fname,'_.txt')
  downloadHandler(fname,function(file) {
    write_lines(paste(unique(list), collapse = ','), file)
  })
}

downloadTableCSV <- function(table2save,fname){
  fname <- paste0(fname,'_.csv')
  downloadHandler(fname,function(file) {
    write.csv(table2save, file, row.names = FALSE)})
}

downloadPlotPNG <- function(plot2save,fname){
  fname <- paste0(fname,'_.png')
  downloadHandler(fname,function(file) {
    ggsave(file, plot = plot2save, device = 'png', width = 400, height = 300, units = 'mm')
  })
}