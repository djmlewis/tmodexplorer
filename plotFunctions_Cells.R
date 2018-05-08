linePlot <- function(data4cell, yLims, cellT, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,leg,zero, splitCells,sem, xgrid, point, freeY) {
  plotCell <-
    ggplot(data = data4cell) +
    ggtitle(cellT) + #paste0('White Blood Cell Responses\n',titles)) +
    themeBase(rotate = FALSE) +
    theme(panel.grid.major.x = element_blank())
  
  if(freeY == FALSE && splitCells == TRUE) {
    plotCell <- plotCell +
      scale_y_continuous(limits = yLims)
  }
  
  if(meanFC == ".FC" && zero == TRUE) {
    plotCell <-  plotCell + geom_hline(yintercept = 1.0, linetype = 2)
  }
  
  plotCell <- plotCell +
    scale_x_continuous(breaks = xbreaks) +
    geom_line(mapping = aes_string(x= "Day", y = yColumn, color = "Cells"), size = 1 ,show.legend=leg)
  
  if(sem) plotCell <- plotCell + geom_ribbon(mapping = aes_string(x= "Day", ymin = paste0("SEML",meanFC), ymax = paste0("SEMU",meanFC), fill = "Cells"), alpha = 0.1,show.legend=leg)
  if(point ==  TRUE) plotCell <- plotCell + geom_point(mapping = aes_string(x= "Day", y = yColumn, color = "Cells"), size = 2 ,show.legend=leg)
  
  if(xgrid == TRUE) {
    plotCell <- plotCell + 
      geom_vline(xintercept = xbreaks, color = 'grey80', alpha = 0.5, show.legend = FALSE) +
      theme(panel.grid.major.y = element_line(color = 'grey80', linetype = 2))
  }
}

plotSelectedCellsSeries <-  function(cellsD,meanFC, vaccs,days,cells,boxlines, titles,leg,zero, splitCells,sem, xgrid, point, freeY,numPlotCols) {
  plot2plot <-  NULL
  data2plot <- NULL
  # cellsD is a list
  if(!is.null(cellsD)) {
    # boxlines == Lines "Mean", Box: "Value"
    data2plot <- cellsD[[boxlines]] %>%
      filter(Treatment %in% vaccs, Day %in% days, Cells %in% cells) %>%
      # preserve order of select menus
      mutate(
        Treatment = factor(Treatment, levels = vaccs),
        Cells = factor(Cells, levels = cells)
      )

    # do any data transforms
    if(boxlines == "Value") {
      data2plot <- mutate(data2plot,Day = factor(Day, levels = days))
    }
    
    treatments <- levels(data2plot$Treatment)
    celltypes <- levels(data2plot$Cells)
    
    yColumn <- paste0(boxlines,meanFC)
    
    # Calc the MAX MIn for Y - have to allow for the SEM ribbon
    if(boxlines == "Mean" && sem == TRUE) {
      yColumnMax <- paste0("SEMU",meanFC)
      yColumnMin <- paste0("SEML",meanFC)
    } else {
      yColumnMax <- yColumn
      yColumnMin <- yColumn
    }
    maxmins <- data2plot %>%
      group_by(Cells) %>%
      summarise(
        # .data calls the supplied data object and allows us to use [[ stringVar ]]
        Max = max(.data[[yColumnMax]],na.rm = TRUE),
        Min = min(.data[[yColumnMin]],na.rm = TRUE)
      )
    maxs <-  set_names(maxmins$Max,maxmins$Cells)
    mins <-  set_names(maxmins$Min,maxmins$Cells)

    if(boxlines == "Mean") {
      # lines plots
      xbreaks <- unique(data2plot$Day)
      
      # start Treatment lapply
      treatList <- lapply(treatments,function(treat) {
        data4cell <- filter(data2plot,Treatment == treat)
        
        if(splitCells == TRUE) {
          # start Cells lapply
          plotsList <- lapply(celltypes,function(cellT) {
            data4cell <- filter(data4cell,Cells == cellT)
            return(linePlot(data4cell,c(mins[[cellT]],maxs[[cellT]]), cellT, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,leg,zero, splitCells,sem, xgrid, point, freeY))
          # end plots lapply
        })
        } else {
          # plot in one go without cells split
          plotsList <- list(linePlot(data4cell,c(NA,NA), NULL, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,leg,zero, splitCells,sem, xgrid, point, freeY))
        }
        
        return(arrangeGrob(grobs = plotsList, ncol = min(numPlotCols, length(plotsList)), top = treat))
        # end treat lapply
      })
      plot2plot <- marrangeGrob(treatList, ncol = 1, nrow = length(treatments), top = NULL)
                    
    } else {
      # box plots
      plot2plot <-
        ggplot(data = data2plot) +
        ggtitle(paste0('White Blood Cell Responses\n',titles)) +
        themeBase(rotate = FALSE) +
        theme(panel.grid.major.x = element_blank())

      
      plot2plot <- plot2plot +
        geom_boxplot(mapping = aes(x = Day, y = yColumn, color = Cells, fill = Cells), alpha = 0.5, outlier.alpha = 1.0,show.legend=leg)      
    }
  
    # if(splitCells == TRUE) {
    #   plot2plot <- plot2plot + facet_wrap(Treatment~Cells, scales = ifelse(freeY,"free_y","fixed"))
    # } else {
    #   plot2plot <- plot2plot + facet_wrap(~Treatment, scales = ifelse(freeY,"free_y","fixed"))
    # }
    
  }
  return(list(plot = plot2plot, table = data2plot))
}