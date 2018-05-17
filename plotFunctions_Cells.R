yTitleForMeanFC <- function(meanFC, boxlines) {
  case_when(
    meanFC == ".FC" ~ "Fold Change",
    boxlines == "Value" ~ "Cell Count",
    boxlines == "Mean" ~ "Mean Cell Count",
    TRUE ~ ""
  )
}
makeLegend <- function(legSum, cells, vaccs,boxlines,point) {
  if(legSum == FALSE) return(NULL)
  df <- expand.grid(Cells = cells, Treatment = vaccs, stringsAsFactors = FALSE)
  df$x <- 1
  df$y <- 2
  p <- ggplot(df,mapping = aes(x = x, y = y)) + 
    themeBase() + 
    theme(legend.position = 'top', legend.direction = 'horizontal',legend.title = element_blank(), legend.text = element_text(size = 16)) +
    scale_color_manual(values = cellsColours) +
    scale_fill_manual(values = cellsColours) +
    scale_shape_manual(values = vaccineShapes)
  
  if (boxlines == "Mean") {
    p <- p +
      geom_line(mapping = aes(color = Cells), size = 1, show.legend = TRUE)
    if (point ==  TRUE) {p <- p + geom_point(mapping = aes(color = Cells, shape = Treatment), fill = 'white', size = 4, show.legend = TRUE)}
    
    # override the aes to remove shapes from cells lines
    p <- p + guides(colour = guide_legend(override.aes = list(shape = NA))) #, shape = guide_legend(override.aes = list(linetype = 0))
    
  } else {
    p <- p + geom_boxplot(mapping = aes(color = Cells, fill = Cells),alpha = 0.5, show.legend = TRUE)
  }
  return(get_legend(p))
}
setupGGplot <- function(data4cell, yLims, cellT, meanFC, zero,freeY,boxlines) {
  plotCell <-
    switch (boxlines,
            "Mean" = ggplot(data = data4cell, mapping = aes(shape = Treatment)) + guides(colour = guide_legend(override.aes = list(shape = NA))),
            "Value" = ggplot(data = data4cell)
    ) +
    ggtitle(cellT) + 
    themeBase(rotate = FALSE) +
    theme(panel.grid.major.x = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 16)) +
    scale_color_manual(values = cellsColours) +
    scale_fill_manual(values = cellsColours) +
    scale_shape_manual(values = vaccineShapes)
  
  if(freeY == FALSE) {
    plotCell <- plotCell +
      scale_y_continuous(limits = yLims)
  }
  
  if(meanFC == ".FC" && zero == TRUE) {
    plotCell <-  plotCell + geom_hline(yintercept = 1.0, linetype = 2)
  }
  return(plotCell)
}
boxPlot <- function(data4cell, yLims, cellT, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles, legAll, zero, splitCells, splitVaccs, sem, xgrid, point, freeY) {
  plotCell <- setupGGplot(data4cell, yLims, cellT, meanFC, zero,freeY,boxlines) +
  geom_boxplot(mapping = aes_string(x = "Day", y = yColumn, color = "Cells", fill = "Cells"), alpha = 0.5, outlier.alpha = 1.0,show.legend=legAll)
  return(plotCell)
}

linePlot <- function(data4cell, yLims, cellT, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY) {
  plotCell <- setupGGplot(data4cell, yLims, cellT, meanFC, zero,freeY,boxlines) +
    scale_x_continuous(breaks = xbreaks) +
    geom_line(mapping = aes_string(x= "Day", y = yColumn, color = "Cells"), size = 1 ,show.legend=legAll)
  
  if(sem) plotCell <- plotCell + geom_ribbon(mapping = aes_string(x= "Day", ymin = paste0("SEML",meanFC), ymax = paste0("SEMU",meanFC), fill = "Cells"), alpha = 0.2,show.legend=FALSE)
  if(point ==  TRUE) plotCell <- plotCell + geom_point(mapping = aes_string(x= "Day", y = yColumn, color = "Cells"), fill = "white", size = 5 ,show.legend=legAll)
  
  if(xgrid == TRUE) {
    plotCell <- plotCell + 
      geom_vline(xintercept = data4cell$Day, color = 'grey80', alpha = 0.5, show.legend = FALSE) +
      theme(panel.grid.major.y = element_line(color = 'grey80', linetype = 2))
  }
  return(plotCell)
}

plotCells <- function(data4cell, numPlotCols,mins,maxs,xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY) {
  if(splitCells == TRUE) {
    # start Cells lapply
    plotsList <- lapply(cells,function(cellT) {
      data4cell <- filter(data4cell,Cells == cellT)
      plotOfCell <- switch (boxlines,
                            "Mean" = linePlot(data4cell, c(mins[[cellT]],maxs[[cellT]]), cellT, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY),
                            "Value" = boxPlot(data4cell, c(mins[[cellT]],maxs[[cellT]]), cellT, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY)
      )
      return(plotOfCell)
      # end plots lapply
    })
  } else {
    # plot in one go without cells split. maxs and mins have only one entry each
    plotOfTreatment <- 
      switch (boxlines,
              "Mean" = linePlot(data4cell, c(mins[["Min"]],maxs[["Max"]]), NULL, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY),
              "Value" = boxPlot(data4cell, c(mins[["Min"]],maxs[["Max"]]), NULL, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY)
      )
    plotsList <- list(plotOfTreatment)
  }
  # end of cells plots
  return(arrangeGrob(grobs = plotsList, ncol = min(numPlotCols, length(plotsList)),
                     top = textGrob(as.character(treat), gp=gpar(fontface="bold",fontsize=20, padding = 4))))
}

plotSelectedCellsSeries <-  function(cellsD,meanFC, vaccs,days,cells,boxlines, titles,legSum, legAll, zero, splitCells, splitVaccs, sem, xgrid, point, freeY,numPlotCols) {
  plot2plot <-  NULL
  data2plot <- NULL
  # cellsD is a list
  if(!is.null(cellsD)) {
    showNotification("Please wait for data table and plot output. This may take a while if many cell types ~ vaccines selected…", type = 'message', duration = 6)
    
    # we must override splitVaccs if it is a boxPlot as we always split boxplots by treatment
    # Do it once here and avoids a lot of extra ifs below
    if(boxlines == "Value") splitVaccs <- TRUE
    # ditto showPoints always if splitVaccs == F
    if(splitVaccs == FALSE) point <- TRUE
    
    # boxlines == Lines "Mean", Box: "Value"
    data2plot <- cellsD[[boxlines]] %>%
      filter(Treatment %in% vaccs, Day %in% days, Cells %in% cells) %>%
      # preserve order of select menus
      mutate(
        Treatment = factor(Treatment, levels = vaccs)
      )
    
    # do any data transforms
    if(boxlines == "Value") {
      data2plot <- mutate(data2plot,Day = factor(Day, levels = days))
    }
    
    yColumn <- paste0(boxlines,meanFC)
    
    treatments <- levels(data2plot$Treatment)

    # Calc the MAX MIn for Y - have to allow for the SEM ribbon
    if(boxlines == "Mean" && sem == TRUE) {
      yColumnMax <- paste0("SEMU",meanFC)
      yColumnMin <- paste0("SEML",meanFC)
    } else {
      yColumnMax <- yColumn
      yColumnMin <- yColumn
    }
    if(splitCells == TRUE) {
      # make a max and min list for each cell type
      maxmins <- data2plot %>%
        group_by(Cells) %>%
        summarise(
          # .data calls the supplied data object and allows us to use [[ stringVar ]]
          Max = max(.data[[yColumnMax]],na.rm = TRUE),
          Min = min(.data[[yColumnMin]],na.rm = TRUE)
        )
      maxs <-  set_names(maxmins$Max,maxmins$Cells)
      mins <-  set_names(maxmins$Min,maxmins$Cells)
    } else {
      # make a max and min list for all data
      maxs = list(Max = max(data2plot[[yColumnMax]],na.rm = TRUE))
      mins = list(Min = min(data2plot[[yColumnMin]],na.rm = TRUE))
    }
    
    # lines plots need this
    xbreaks <- unique(data2plot$Day)
    
    # force a special case trough if neither Treatment or Cells split
    if(splitCells == FALSE && splitVaccs == FALSE) {
      treatList <- list(
        switch (boxlines,
                "Mean" = linePlot(data2plot, c(mins[["Min"]],maxs[["Max"]]), NULL, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY),
                "Value" = boxPlot(data2plot, c(mins[["Min"]],maxs[["Max"]]), NULL, xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY)
        ))
    } else {
      if(splitVaccs == TRUE) {
        # splitCells may be T or F
        # start Treatment lapply
        treatList <- lapply(treatments,function(treat) {
          data4cell <- filter(data2plot,Treatment == treat)
          return(plotCells(data4cell,numPlotCols,mins,maxs,xbreaks, yColumn, treat, meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY))
        })# end of treatments lapply
      } else {
        # splitVaccs == False and splitCells must be True
        treatList <- list(plotCells(data2plot,numPlotCols,mins,maxs,xbreaks, yColumn, "", meanFC, vaccs,days,cells,boxlines, titles,legAll, zero, splitCells, splitVaccs,sem, xgrid, point, freeY))
      }
    }
    
    # if splitCells == F then we can arrange the treatments on a row using numPlotCols, otherwise keep ncol = 1
    ncols <- ifelse(splitCells == TRUE,1,min(numPlotCols, length(treatList)))
    # marrangeGrob demands nrow and ncol to be specified which is tricky to calculate, so leave at 1,1
    # instead, harness the flexibility of arrangeGrob to flow the rows according to ncol. 
    arrangedTreats <- list(arrangeGrob(grobs = treatList, ncol = ncols))
    plot2plot <- marrangeGrob(arrangedTreats, ncol = 1, nrow = 1, 
                              top = makeLegend(legSum,cells, vaccs,boxlines,point), 
                              bottom = textGrob("Days After Immunisation", gp=gpar(fontsize=16)),
                              padding = unit(0.5, "line"),
                              left = textGrob(yTitleForMeanFC(meanFC, boxlines), gp=gpar(fontsize=16), rot = 90)
    )
  }
  return(list(plot = plot2plot, table = data2plot))
}