getCytokinesData2Plot <- function(data2plot, cyts, days, acts) {
  if (is.null(data2plot) || nrow(data2plot) == 0) return(NULL)
  
  data2plot <- data2plot %>%
    filter(CYTOKINE %in% cyts, DAY %in% days, ACTARMCD %in% acts) %>%
    mutate_at(vars(CYTOKINE, DAY,ACTARMCD),as.factor) %>%
    group_by(ACTARMCD,DAY,CYTOKINE)
  
  return(data2plot)
}

ggplotCytokinesForTreatmentDayViolin <- function(data2plot) {
    if (is.null(data2plot) || nrow(data2plot) == 0)
      return(NULL)

    plot <- ggplot(data = data2plot, mapping = aes(x = DAY, y = VALUE))
    
    plot <- plot +
      # geom_boxplot(mapping = aes(colour = CYTOKINE, fill = CYTOKINE), alpha = 0.4) + #,
      geom_violin(mapping = aes(colour = CYTOKINE, fill = CYTOKINE), alpha = 0.4) + #,
      facet_wrap(CYTOKINE ~ ACTARMCD, scales = 'free_y')
    
    return(plot)
  }