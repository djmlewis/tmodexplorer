
themeBase <- function(rotate = FALSE) {
  a <- ifelse(rotate,60,0)
  hj <- ifelse(rotate,1,0.5)

  t <- theme_bw() + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#f8ffeb"),
    axis.title = element_blank(),
    strip.text = element_text(size = 14),
    axis.line.x = element_line(size = 1),
    axis.line.y = element_line(size = 1),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, angle = a, hjust = hj),
    legend.title = element_text()
  )
  return(t)
}

plotBaseBoxplot <- function(x,y,s,t,z,l,xmax,xmin,naGenes){

  cexAxis = 1.5
  ncols <- length(levels(x))
  colpal <- rainbow(ncols, alpha = 0.4)
  bordpal <- rainbow(ncols)
  insetv <- -0.16*cexAxis
  colN <- 20
  legcols <- (ncols %/% colN) + 1
  # dont you just love fudge factors?
  sw <- max(strwidth(x, units = "inches"))
  lmar <- (sw+0.6)*cexAxis
  rmar <- ifelse(l == TRUE,sw*legcols+1.2*cexAxis,0.8)
  if(lmar+rmar>par("pin")[1]) rmar <- ifelse(l == TRUE,par("pin")[1]-lmar-1,0.8)
  
  original.parameters<- par( no.readonly = TRUE )
  par(mai = c(0.7, lmar, 0.8, rmar))
  {
    # to force a zero line we have to set ylim to 0 as needed
    ymin <- ifelse(z == TRUE, min(min(y),0),min(y) )
    boxplot(y ~ x, col = colpal, medcol = bordpal, border = "black", pars = list(las = 1, cex.axis = cexAxis), horizontal = TRUE, outline = TRUE, ylim = c(ymin,max(y)))
    if(!is.null(s)) text(y = x, x = min(ymin,xmin), labels = s, pos = 2)
    if(length(naGenes)>0) text(y = naGenes, x = min(ymin,xmin), labels = c('Missing'), pos = 4)
    title(t)
    if(z == TRUE) abline(v = 0.0, xpd = FALSE, col = "gray60", lty = 'dashed')
    if(l == TRUE) legend(x = xmax+xmax/colN, y = ncols+(ncols/colN),bty = 'n',ncol = legcols, horiz = FALSE, inset = c(insetv,0), legend = levels(x), fill = colpal, xpd = TRUE)
  }
  plot <- recordPlot()
  par(original.parameters)
  # actually the return is unusable. Plotting happens at once
  return(plot)
}

plotGenesModules <- function(d,t,l,z,gg,grouper){
  plot <-  NULL
  if (!is.null(d) && nrow(d) > 0) {
     d <- d %>%
      filter(!is.na(Value)) %>%
      mutate(Module = droplevels(Module), 
             Title = as.factor(Title))# gsub(" ","\n",)
    
    if(grouper == "Title") {
      # we have to reorder as data is arranged by mean according to module
      d$Title <- fct_reorder(d$Title,d$Value,median)
    }
    
    if(gg == FALSE){
      xmax <- max(d$Value,na.rm = TRUE)
      xmin <- min(d$Value,na.rm = TRUE)
      plot <- plotBaseBoxplot(d[[grouper]],d$Value,NULL,paste0('Modules For Selected Genes\n',t),z,l,xmax,xmin,c())
    } else {
      plot <-  ggplot(
        data = d,
        mapping = aes_string(
          x = grouper,
          y = "Value",
          colour = grouper,
          fill = grouper
        )
      )
      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
      plot <-  plot +
        geom_boxplot(alpha = 0.5, outlier.alpha = 1.0,show.legend=l) + coord_flip() +
        ggtitle(paste0('Modules For Selected Genes\n',t)) +
        themeBase(FALSE)
    }
  }
  return(plot)
}


plotModuleGenes <- function(d,m,t,l,z,gg,showmiss) {
  plot <- NULL
  if (!is.null(d) && nrow(d) > 0) {
    if(showmiss == TRUE) {
    NAfactors <- d %>%
      filter(is.na(Value)) %>%
      select(Gene) %>%
      distinct(Gene)
    } else {
      NAfactors <- data.frame(Gene = character())
    }
    # now remove NAs to allow min max etc
    d <- d %>%
      filter(!is.na(Value)) #%>%
      #mutate(Gene = droplevels(Gene)) Dont drop levels, we see them as empty - labelled "missing"

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
      geom_boxplot(mapping = aes(y = Value),alpha = 0.5, outlier.alpha = 1.0, show.legend=l) +
      scale_x_discrete(drop = !showmiss) +
      coord_flip() +
      ggtitle(paste0('Genes for module ',m,'\n',t)) +
      themeBase(FALSE)
      
      if(nrow(NAfactors)>0){
        plot <- plot +
          geom_text(data = NAfactors, mapping = aes(x = Gene), inherit.aes = FALSE, size = 5, label = "Missing", y = xmin, hjust = 0, show.legend=FALSE)
      }
      
      if(z == TRUE) {
        plot <-  plot + geom_hline(yintercept = 0.0, linetype = 2)
      }
    } else {
      if(showmiss == TRUE) {
        nafactors <- NAfactors$Gene
      } else {
        d <- d %>%
          mutate(Gene = droplevels(Gene))
        nafactors <- c()
      }
      plot <- plotBaseBoxplot(d$Gene,d$Value,d$Selected,paste0('Genes for module ',m,'\n',t),z,l,xmax,xmin,nafactors)
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
    geom_text(data = sortColDF, mapping = aes(x = Column),label = "▼", color = 'red', y = Inf, size = 5, hjust = 0.5, vjust = 1, show.legend=FALSE) +
    geom_text(data = sortColDF, mapping = aes(x = Column),label = "▲", color = 'red', y = -Inf, size = 5, hjust = 0.5, vjust = 0, show.legend=FALSE)
  
  return(plot)
}

plotTopGenesInSeries <- function(data2plot,
           showPoints,
           showSEM,
           showlegend,
           t,
           facet,
           showZero,pointsBoxes,sortCol,xgrid,
           splitGenes,
           numCols,
           kinetics, showkinetics) {
    if (is.null(data2plot) || nrow(data2plot) == 0) return(NULL)
  
  showNotification("Please wait for plot output…", type = 'message', duration = 3)
  
  # asGenes  detect whether it really is as genes based on genesOrProbes: cannot rely on lacking column ProbeName 
  asGenes <- get("genesOrProbes", envir = .GlobalEnv) == "Gene" # ('ProbeName' %in% names(data2plot) == FALSE)
  
    if (asGenes) {
      # leave genes alone
      plotData <- data2plot
    } else {
      # merge gene and probe names if not averaged
      plotData <- data2plot %>%
        mutate(Gene = paste0(Gene, ' (', ProbeName, ')')) #%>%
        #select(-c(ProbeName))
    }
  
  # since we added 306B we have rows with NA values so filter them out
  plotData <- filter(plotData,!is.na(Value))

    plotData <- plotData %>%
      mutate(Gene = factor(Gene, levels = unique(Gene)))
    
    if(facet == FALSE) {
      plotData <- plotData %>%
        mutate(Column = factor(Column, levels = unique(Column)))# unique() works - do not sort
    }
    

    plot <-   ggplot(data = plotData) +
      themeBase(facet == FALSE)  +
      ggtitle(paste0('Selected ',ifelse(asGenes,'Genes','Probes'),'\n', t))
    
    
    if (showZero == TRUE) {
      plot <- plot + geom_hline(yintercept = 0.0, linetype = 2)
    }
    
    if(xgrid == TRUE && pointsBoxes == 'Lines' && facet == TRUE) {
        plot <- plot + geom_vline(mapping = aes(xintercept = Column), color = 'grey80', alpha = 0.5, show.legend = FALSE) +
          theme(panel.grid.major.y = element_line(color = 'grey80', linetype = 2))
    }
    
    
    if(pointsBoxes == 'Boxplot') {
      if(facet == TRUE) {
        plot <- plot + 
          scale_color_manual(values = vaccineColours) +
          scale_fill_manual(values = vaccineColours) +
          geom_boxplot(mapping = aes(x = Column, y = Value, group = Column, colour = Treatment, fill = Treatment), alpha = 0.5, outlier.alpha = 1.0, show.legend = showlegend)
      } else {
        plot <- plot + 
        geom_boxplot(mapping = aes(x = Column, y = Value, group = Column), colour = 'black', fill = 'black', alpha = 0.5, outlier.alpha = 1.0, show.legend = FALSE)
      }
    }
    
    if(pointsBoxes == 'Lines') {
      if(asGenes == FALSE || (asGenes == TRUE && splitGenes == FALSE)) {
        plot <- plot + 
          geom_line(mapping = aes(x = Column,y = Value,colour = Gene,group = Gene), size = 1, show.legend = showlegend) + # group = Gene is needed when we do not facet
        {if(showSEM == TRUE && asGenes == TRUE){geom_ribbon(mapping = aes(x = Column, ymin = Value-SEM, ymax = Value+SEM, fill = Gene), alpha = 0.2, show.legend = showlegend)}} +
        {if(showPoints == TRUE){geom_point(mapping = aes(x = Column,y = Value,colour = Gene,fill = Gene,group = Gene), size = 2 ,show.legend = showlegend)}}
      } else { # split genes to probes
        dataWithShapes <- data2plot
        if(showPoints == TRUE){
          dataWithShapes <- data2plot %>%
            distinct(Gene,ProbeName) %>%
            group_by(Gene) %>%
            # use rep_len in ulikely event that n()>20
            mutate(Shape = rep_len(0:19,length.out = n())) %>%
            ungroup() %>%
            mutate(Shape = as.factor(Shape)) %>%
            full_join(data2plot,by = c("Gene", "ProbeName"))
          
          showNotification("Shapes have been added to points to distinguish probes mapping the same gene")
          
          plot <- plot + 
            geom_point(data = dataWithShapes,  mapping = aes(x = Column,y = Value,colour = Gene,fill = Gene,group = ProbeName, shape = Shape), size = 4 ,show.legend = FALSE) +
            scale_shape_manual(values = as.integer(levels(dataWithShapes$Shape)), guide = 'none')
        }
        plot <- plot + 
          geom_line(data = dataWithShapes,  mapping = aes(x = Column,y = Value,colour = Gene,group = ProbeName), size = 1, show.legend = showlegend) # group = Gene is needed when we do not facet
      }
    }
    
    if (facet == TRUE) {
      plot <-  plot +
        scale_x_continuous(breaks = plotData$Column) +
        facet_wrap( ~ Treatment, ncol = numCols)
    }
    
    #sortCol - VACCINE_DAY
    if (sortCol %in% data2plot$Column || (facet == TRUE &&(unlist(str_split(sortCol, "_"))[1] %in% unique(data2plot$Treatment) &&
                                                           unlist(str_split(sortCol, "_"))[2] %in% unique(data2plot$Column)))) {
      plot <- addSortColPlot(sortCol,facet,plot,levels(data2plot$Column))
    }
    
    if(showkinetics == TRUE && !is.null(kinetics) && pointsBoxes == 'Lines' && facet == TRUE) {
      df <-  kineticsDF(kinetics, TRUE) %>%
        filter(Day %in% data2plot$Column)
      plot <- plot +
        geom_rect(data = df, mapping = aes(xmin = Day-0.2, xmax = Day+0.2, ymin = Min, ymax = Max), fill = NA, color = 'grey20', alpha = 1, show.legend = FALSE)
    }
    
    return(plot)
  }

plotModulesInSeries <- function(d,t,l,r,f,z,se,sC,xg,pp,numCols){
  p <-  NULL
  if (!is.null(d) && nrow(d) > 0) {
    showNotification("Please wait for plot output…", type = 'message', duration = 3)

    p <- ggplot(data = d, mapping = aes(x = Column)) +
      ggtitle(paste0('Modules For Selected Genes / Probes\n',t)) +
      themeBase(f == FALSE)
 

    if(xg == TRUE && r == 'Lines' && f == TRUE) {
      p <- p + geom_vline(mapping = aes(xintercept = Column), color = 'grey80', alpha = 0.5, show.legend = FALSE) +
        theme(panel.grid.major.y = element_line(color = 'grey80', linetype = 2))
    }
    
    if(z == TRUE) {
      p <- p +
        geom_hline(yintercept = 0.0, linetype = 2)
    }
    
    if(r == 'Lines'){
      if(length(unique(d$Column)) > 1) {
        p <- p +
        {if(se == TRUE){geom_ribbon(mapping = aes(ymin = SElo, ymax = SEhi, fill = Module, group = Module), alpha = 0.2,show.legend=l)}} +
        {if(pp == TRUE){geom_point(aes(y = Value, colour = Module, group = Module), size = 2 ,show.legend=l)}} +
          geom_line(aes(y = Value, colour = Module, group = Module), size = 1,show.legend=l)
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
        geom_boxplot(mapping = aes(y = Value, colour = Module, fill = Module), alpha = 0.5, outlier.alpha = 1.0,show.legend=l)
    }
    
    if(f == TRUE){
      p <- p +
        facet_wrap(~Treatment, ncol = numCols)
    }
    
    #sortCol - VACCINE_DAY must come last or factors go awry
    if (sC %in% d$Column || (f == TRUE && (unlist(str_split(sC, "_"))[1] %in% unique(d$Treatment) &&
                                          unlist(str_split(sC, "_"))[2] %in% unique(d$Column)))) {
      p <- addSortColPlot(sC,f,p,levels(d$Column))
    }
    
  }
  return(p)
}


getTopGenesInSeriesToPlotWithModules <- function(allData, topGenes,selCols,facetted,boxRibbon,moduleValues) {
  #getTopGenesInSeries needs genesProbesSelected so we supply all in topGenes. topGenes is topGenesAndModules()[['genes']]
  #so we have to supply Gene or ProbeName for the column based on get("genesOrProbes", envir = .GlobalEnv)
  # we set the splitProbes to FALSE so we get gene means if we are using them
  topGenesInSeries <- getTopGenesInSeries(allData,topGenes,selCols, facetted, unique(topGenes[[get("genesOrProbes", envir = .GlobalEnv)]]), FALSE) %>%
    # need to add a psuedo module
    mutate(Module = 'Selected')
  # drop probe if we have it
  if('ProbeName' %in% names(topGenesInSeries) == TRUE) {
    topGenesInSeries <- topGenesInSeries %>%
      select(-ProbeName)
  }

  # dont factor if it is facetted and a ribbon, always factor boxplots 
  if(facetted == TRUE && boxRibbon == "Boxplot") {
    topGenesInSeries <- topGenesInSeries %>%
      mutate(Column = as.factor(Column))# integers so as.factor
  } 
  if(facetted == FALSE) { #and when unfacetted
    topGenesInSeries <- topGenesInSeries %>%
      mutate(Column = factor(Column, levels = levels(moduleValues$Column)))
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
        select(Treatment, Module, Column,  Value = Mean, N, SD, SElo, SEhi)
    } else {
      topGenesInSeries <- topGenesInSeries %>%
        select(Module, Column,  Value = Mean, N, SD, SElo, SEhi) %>%
        arrange(Module, Column)
    }
  }
  
  # Treatment is a factor, so factor our order to match the Treatment inputSelect order if as Lines - fudge..
  # Note fct_relevel instead of factor() gives an obscure warning 
  # topGenesInSeries <- getTopGenesInSeries has used the selCols and returns topGenesInSeries$Treatment with the correct levels so use those
  # 
  if(facetted == TRUE && boxRibbon == "Lines") {
    moduleValues <- moduleValues %>%
      mutate(Treatment = factor(Treatment, levels = levels(topGenesInSeries$Treatment))) %>%
      select(Treatment, Module, Column, everything()) %>%
      arrange(Treatment, Module, Column)
  }
  
  # join our psuedo module to the others
  moduleValues <- moduleValues %>%
    bind_rows(topGenesInSeries)
  
  return(moduleValues)
}

plotDataTable <- function(data2plot,file, widthFactor) {
  # plot <-  NULL
  if (is.null(data2plot) || nrow(data2plot) < 1) {
    png(file)
    grid.newpage()
    grid.text("No Modules Matched")
    dev.off()
  } else {
    th <- ttheme_default(
      core=list(bg_params = list(fill = c('#feffee','white'), col=NA),fg_params=list(hjust=0, x=0.1)),
      colhead=list(bg_params = list(fill = c('#d0e862'), col=NA),fg_params=list(hjust=0, x=0.1))
    )
    t <- tableGrob(data2plot, rows = NULL, theme = th)
    h <- convertHeight(grobHeight(t),'mm', valueOnly = TRUE)
    w <- convertHeight(grobWidth(t),'mm', valueOnly = TRUE)
    
    png(file, height = h*1.8, width = w*widthFactor, units = 'mm', res = 300, bg = "transparent")
    grid.newpage()
    grid.draw(t)
    dev.off()
    
  }
}

printPlotPNG <- function(plot2plot,file, h,w) {
  if (is.null(plot2plot)) {
    png(file)
    grid.newpage()
    grid.text("Nothing To Plot")
    dev.off()
  } else {
    # we set res in renderplot as 72, so we have to scale-up the h,w to get higher resolution
    png(file, height = h*300/72, width = w*300/72, res = 300, units = 'px', bg = "white")
    print(plot2plot)
    dev.off()
    
  }
}

plotPlotPNG <- function(plot2plot,file, h,w, gridDraw) {
  if (is.null(plot2plot)) {
    png(file)
    grid.newpage()
    grid.text("Nothing To Plot")
    dev.off()
  } else {
    # we set res in renderplot as 72, so we have to scale-up the h,w to get higher resolution
    png(file, height = h*300/72, width = w*300/72, res = 300, units = 'px', bg = "white")
    switch(gridDraw,
      "v" = grid.draw(plot2plot),
      plot(plot2plot))
    dev.off()
  }
}

getGGplotShapeMiniplot <- function(kinetics,dataValueRange) {
  if(is.null(kinetics)) return(NULL)
  df <-  kineticsDF(kinetics) %>%
    mutate(DayF = 1:length(Day))
    
    plot <- ggplot(df, aes(xmin = DayF-0.4, xmax = DayF+0.4, ymin = Min, ymax = Max)) +
      theme(axis.title = element_blank()) +
      scale_x_continuous(labels = as.character(df$Day),breaks = df$DayF) +
      geom_rect(mapping = aes(fill = Exclude, color = Exclude), alpha = 0.3, show.legend = FALSE) +
      geom_point(aes(x = DayF, y = Y), shape = 8, size = 3) +
      scale_fill_manual(values = c(`TRUE` = "white", `FALSE` = "#f9b800")) +
      scale_color_manual(values = c(`TRUE` = "grey20", `FALSE` = "#f9b800")) +
      geom_hline(yintercept = c(dataValueRange[["Min"]],dataValueRange[["Max"]]), linetype = 2, color = 'grey50')
  
  return(plot)
}

