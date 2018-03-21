colours <- c(
  "TNFalpha" = 'lightsalmon',
  "IL12p40"  = 'brown4',
  "CCL5" = 'coral3',
  "IL8"  = 'red',
  "MCP1"  = 'tomato',
  "IFNgamma" = 'orange',
  "MIP1alpha" = 'gold',
  "IL1alpha" = 'gray',
  "GMCSF"  = 'yellow',
  "IP10"  = 'olivedrab1',
  "TNFR1" = 'green',
  "IL6"  = 'darkgreen',
  "IL10" = 'deepskyblue',
  "VEGF" = 'turquoise3',
  "PTX3" = 'slateblue3',
  "IL2"  = 'mediumorchid',
  "IL5" = 'magenta2',
  "IL2Ra" = 'deeppink',
  "IFNalpha2" = 'seagreen1',
  "IL1ra" = 'cyan1',
  "TREM1" = 'plum',
  'black' = 'black'
)

vaccinesToPlot_N <-
  c(
    "FLUAD.GENT" = 114,
    "PLACEBO.GENT" = 6,
    "FLUAD.SURREY" = 20,
    "AGRIPPAL" = 21,
    "STAMARIL" = 20,
    "VARILRIX" = 20,
    "ENGERIXB1" = 21,
    "ENGERIXB3" = 20,
    "PLACEBO.AB1C" = 20,
    "PLACEBO.B3" = 20
  )


getCytokinesData2Plot <- function(data2plot, cyts, days, acts) {
  if (is.null(data2plot) || nrow(data2plot) == 0) return(NULL)
  
  data2plot <- data2plot %>%
    filter(CYTOKINE %in% cyts, DAY %in% days, ACTARMCD %in% acts) %>%
    mutate_at(vars(CYTOKINE, DAY,ACTARMCD),as.factor) %>%
    group_by(ACTARMCD,DAY,CYTOKINE)
  
  return(data2plot)
}

ggplotCytokinesForTreatmentDay <- function(data2plot,wrap) {
    if (is.null(data2plot) || nrow(data2plot) == 0)
      return(NULL)

    plot <- ggplot(data = data2plot, mapping = aes(x = DAY, y = VALUE))
    
    plot <- plot +
      # geom_boxplot(mapping = aes(colour = CYTOKINE, fill = CYTOKINE), alpha = 0.4) + #,
      geom_violin(mapping = aes(colour = CYTOKINE, fill = CYTOKINE), alpha = 0.4) + #,
      scale_color_manual(values = colours) +
      scale_fill_manual(values = colours)
    
    switch (wrap,
            'N' = {},
            'V' = {plot <- plot+facet_wrap( ~ ACTARMCD, scales = 'free_y')},
            'VC' = {plot <- plot+facet_wrap(ACTARMCD ~ CYTOKINE, scales = 'free_y')},
            'C' = {plot <- plot+facet_wrap(~ CYTOKINE, scales = 'free_y')},
            'CV' = {plot <- plot+facet_wrap(CYTOKINE ~ ACTARMCD, scales = 'free_y')}
    )
    
    return(plot)
  }