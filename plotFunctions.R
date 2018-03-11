themeBase <- theme_bw() + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#f8ffeb"),
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
      )
      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
      plot <-  plot +
        geom_boxplot(alpha = 0.2, outlier.alpha = 1.0,show.legend=l) + coord_flip() +
        ggtitle(paste0('Modules For Selected Genes\n',t)) +
        themeBase
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

addSortColPlot <- function(sortCol,facet,plot,ll) {
  if(facet == TRUE) {
    # we have a Treatment and Column 
    dy = as.integer(unlist(str_split(sortCol, "_"))[2])
    # levels returns NULL if the facet is lines using integers
    if(!is.null(ll)) {
      dy = factor(dy, levels = ll)
    }
    sortColDF <- data.frame(Treatment = unlist(str_split(sortCol, "_"))[1], 
                            Column = dy)
  } else {
    sortColDF <- data.frame(Column = sortCol)
  }
  
  plot <- plot + 
    geom_text(data = sortColDF, mapping = aes(x = Column),label = "▼", color = 'red', y = Inf, size = 6, hjust = 0.5, vjust = 1, show.legend=FALSE) +
    geom_text(data = sortColDF, mapping = aes(x = Column),label = "▲", color = 'red', y = -Inf, size = 6, hjust = 0.5, vjust = 0, show.legend=FALSE)
  
  return(plot)
}

plotTopGenesInSeries <- function(data2plot,
           showPoints,
           showlegend,
           t,
           facet,
           showZero,pointsBoxes,sortCol,xgrid) {
    if (is.null(data2plot)) return(NULL)
  
  showNotification("Please wait for plot output…", type = 'message', duration = 3)
  
  # asGenes  detect whether it really is as genes based on the selData: if that lacks column Probe then it is
  asGenes <- ('Probe' %in% names(data2plot) == FALSE)
  
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
    
    if(facet == FALSE) {
      plotData <- plotData %>%
        mutate(Column = factor(Column, levels = unique(Column)))
    }
    
    plot <-   ggplot(data = plotData) +
      {if(pointsBoxes == 'Boxplot' && facet == TRUE) {geom_boxplot(mapping = aes(x = Column, y = Value, group = Column, colour = Treatment, fill = Treatment), alpha = 0.2, outlier.alpha = 1.0, show.legend = showlegend)}} +
      {if(pointsBoxes == 'Boxplot' && facet == FALSE) {geom_boxplot(mapping = aes(x = Column, y = Value, group = Column), colour = 'black', fill = 'black', alpha = 0.2, outlier.alpha = 1.0, show.legend = FALSE)}} +
      {if (pointsBoxes == 'Lines') {geom_line(mapping = aes(x = Column,y = Value,colour = Gene,group = Gene), show.legend = showlegend)}} +
      {if(pointsBoxes == 'Lines' && showPoints){geom_point(mapping = aes(x = Column,y = Value,colour = Gene,fill = Gene,group = Gene), show.legend = showlegend)}} +
      ggtitle(paste0('Selected ',ifelse(asGenes,'Genes','Probes'),'\n', t)) +
      themeBase

    if (showZero == TRUE) {
      plot <- plot + geom_hline(yintercept = 0.0, linetype = 2)
    }

    if (facet == TRUE) {
      plot <-  plot +
        scale_x_continuous(breaks = plotData$Column) +
        facet_wrap( ~ Treatment)
    }
    
    if(xgrid == TRUE && pointsBoxes == 'Lines' && facet == TRUE) {
      plot <- plot + geom_vline(xintercept = unique(data2plot$Column), color = 'grey80', alpha = 0.5, show.legend = FALSE)
    }
    
    #sortCol - VACCINE_DAY
    if (sortCol %in% data2plot$Column || (facet == TRUE &&(unlist(str_split(sortCol, "_"))[1] %in% unique(data2plot$Treatment) &&
          unlist(str_split(sortCol, "_"))[2] %in% unique(data2plot$Column)))) {
      plot <- addSortColPlot(sortCol,facet,plot,levels(data2plot$Column))
    }
    return(plot)
  }

plotModulesInSeries <- function(d,t,l,r,f,z,se,sC,xg,pp){
  p <-  NULL
  if (!is.null(d) && nrow(d) > 0) {
    showNotification("Please wait for plot output…", type = 'message', duration = 3)

    p <- ggplot(data = d, mapping = aes(x = Column)) +
      ggtitle(paste0('Modules For Selected Genes / Probes\n',t)) +
      themeBase
    
    if(z == TRUE) {
      p <- p +
        geom_hline(yintercept = 0.0, linetype = 2)
    }
    
    if(r == 'Lines'){
      if(length(unique(d$Column)) > 1) {
        p <- p +
        {if(se == TRUE){geom_ribbon(mapping = aes(ymin = SElo, ymax = SEhi, fill = Module, group = Module), alpha = 0.2,show.legend=l)}} +
        {if(pp == TRUE){geom_point(aes(y = Value, colour = Module, group = Module),show.legend=l)}} +
          geom_line(aes(y = Value, colour = Module, group = Module),show.legend=l)
      } else {# cannot plot lines and ribbons with only 1 point
        p <- p +
          geom_point(aes(y = Value, colour = Module, group = Module),show.legend=l)
      }
      
      # if facet_wrap we split column into real x values. If not we have factors. So only add scale_x_continuous to facet_wrap
      if(f == TRUE) {
        p <- p +scale_x_continuous(breaks = unique(d$Column))
      }
    } else { # boxplot
      p <- p +
        geom_boxplot(mapping = aes(y = Value, colour = Module, fill = Module), alpha = 0.2, outlier.alpha = 1.0,show.legend=l)
    }
    
    if(f == TRUE){
      p <- p +
        facet_wrap(~Treatment)
    }
    
    #sortCol - VACCINE_DAY
    if (sC %in% d$Column || (f == TRUE &&(unlist(str_split(sC, "_"))[1] %in% unique(d$Treatment) &&
          unlist(str_split(sC, "_"))[2] %in% unique(d$Column)))) {
      p <- addSortColPlot(sC,f,p,levels(d$Column))
    }
    if(xg == TRUE && r == 'Lines' && f == TRUE) {
      p <- p + geom_vline(xintercept = unique(d$Column), color = 'grey80', alpha = 0.5, show.legend = FALSE)
    }
    
  }
  return(p)
}


getTopGenesInSeriesToPlotWithModules <- function(allData, topGenes,selCols,facetted,boxRibbon,moduleValues) {
  topGenesInSeries <- getTopGenesInSeries(allData,topGenes,selCols, facetted) %>%
    # need to add a psuedo module
    mutate(Module = 'Selected')
  # drop probe if we have it
  if('Probe' %in% names(topGenesInSeries) == TRUE) {
    topGenesInSeries <- topGenesInSeries %>%
      select(-Probe)
  }
  # dont factor if it is facetted and a ribbon, always factor boxplots 
  if(facetted == TRUE && boxRibbon == "Boxplot") {
    topGenesInSeries <- topGenesInSeries %>%
      mutate(Column = as.factor(Column))
  } 
  if(facetted == FALSE) { #and when unfacetted
    topGenesInSeries <- topGenesInSeries %>%
      mutate(Column = factor(Column, levels = selCols))
  }
  
  if(boxRibbon == "Lines") {
    # this is horrible because we have already split the Column and so have to group_by and Select accordingly
    if(facetted == TRUE) {
      topGenesInSeries <- topGenesInSeries %>%
        group_by(Treatment, Column,Module)
    } else {
      topGenesInSeries <- topGenesInSeries %>%
        group_by(Column,Module)
    }
    topGenesInSeries <- topGenesInSeries %>%
      summarise(
        N = n(),
        SD = sd(Value,na.rm = TRUE),
        Mean = mean(Value,na.rm = TRUE),
        SElo = Mean-SD/sqrt(N),
        SEhi = Mean+SD/sqrt(N)
      ) %>%
      ungroup()
    # horrible again
    if(facetted == TRUE) {
      topGenesInSeries <- topGenesInSeries %>%
        select(Treatment, Column, Module, Value = Mean, N, SD, SElo, SEhi)
    } else {
      topGenesInSeries <- topGenesInSeries %>%
        select(Column, Module, Value = Mean, N, SD, SElo, SEhi)
    }
  }
  
  # join our psuedo module to the others
  moduleValues <- moduleValues %>%
    bind_rows(topGenesInSeries)
  return(moduleValues)
}

