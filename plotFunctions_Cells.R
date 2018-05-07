plotSelectedCellsSeries <-  function(cellsD,meanFC, vaccs,days,cells,boxlines, titles,leg,zero, splitCells,sem, xgrid, point, freeY) {
  plot2plot <-  NULL
  data2plot <- NULL
  # cellsD is a list
  if(!is.null(cellsD)) {
    # boxlines == Lines "means", Box: "all"
    data2plot <- cellsD[[boxlines]] %>%
      filter(Treatment %in% vaccs, Day %in% days, Cells %in% cells) %>%
      # preserve order of select menus
      mutate(
        Treatment = factor(Treatment, levels = vaccs),
        Cells = factor(Cells, levels = cells)
      )

    # do any data transforms
    if(boxlines == "all") {
      data2plot <- mutate(data2plot,Day = factor(Day, levels = days))
    }
    
    plot2plot <-
      ggplot(data = data2plot) +
      ggtitle(paste0('White Blood Cell Responses\n',titles)) +
      themeBase(rotate = FALSE) +
      theme(panel.grid.major.x = element_blank())
    
    if(boxlines == "means") {
      xbreaks <- unique(data2plot$Day)
      
      if(meanFC == ".FC" && zero == TRUE) {
        plot2plot <-  plot2plot + geom_hline(yintercept = 1.0, linetype = 2)
      }
      
      plot2plot <- plot2plot +
        scale_x_continuous(breaks = xbreaks) +
        geom_line(mapping = aes_string(x= "Day", y = paste0("MEAN",meanFC), color = "Cells"), size = 1 ,show.legend=leg)
      
      if(sem) plot2plot <- plot2plot + geom_ribbon(mapping = aes_string(x= "Day", ymin = paste0("SEML",meanFC), ymax = paste0("SEMU",meanFC), fill = "Cells"), alpha = 0.1,show.legend=leg)
      if(point ==  TRUE) plot2plot <- plot2plot + geom_point(mapping = aes_string(x= "Day", y = paste0("MEAN",meanFC), color = "Cells"), size = 2 ,show.legend=leg)
      
      if(xgrid == TRUE) {
        plot2plot <- plot2plot + 
          geom_vline(xintercept = xbreaks, color = 'grey80', alpha = 0.5, show.legend = FALSE) +
          theme(panel.grid.major.y = element_line(color = 'grey80', linetype = 2))
      }
    } else {
      plot2plot <- plot2plot +
        geom_boxplot(mapping = aes(x = Day, y = Value.FC, color = Cells, fill = Cells), alpha = 0.5, outlier.alpha = 1.0,show.legend=leg)      
    }
  
    if(splitCells == TRUE) {
      plot2plot <- plot2plot + facet_wrap(Treatment~Cells, scales = ifelse(freeY,"free_y","fixed"))
    } else {
      plot2plot <- plot2plot + facet_wrap(~Treatment, scales = ifelse(freeY,"free_y","fixed"))
    }
    
  }
  return(list(plot = plot2plot, table = data2plot))
}