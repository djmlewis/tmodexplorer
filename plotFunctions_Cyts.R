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

cytokineLevels <- sort(c(
  "TNFalpha",
  "IL12p40",
  "CCL5",
  "IL8",
  "MCP1",
  "IFNgamma",
  "MIP1alpha",
  "IL1alpha",
  "GMCSF",
  "IP10",
  "TNFR1",
  "IL6",
  "IL10",
  "VEGF",
  "PTX3",
  "IL2",
  "IL5",
  "IL2Ra",
  "IFNalpha2",
  "IL1ra",
  "TREM1"
))

cytokineColours <- setNames(rainbow(length(cytokineLevels)), cytokineLevels)

vaccineLevels <-
  sort(c(
    "FLUAD.GENT",
    "PLACEBO.GENT",
    "FLUAD.SURREY",
    "AGRIPPAL",
    "STAMARIL",
    "VARILRIX",
    "ENGERIXB1",
    "ENGERIXB3",
    "PLACEBO.AB1C",
    "PLACEBO.B3"
  ))

vaccinesToPlot_N <-
  sort(c(
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
  ))


getCytokinesDataAndPlot <- function(cdp, data2plot, cyts, days, acts, wrap, plottype) {
  if (is.null(data2plot) || nrow(data2plot) == 0) return(list(data = NULL, plot = NULL))
  
  dataFiltered <- data2plot %>%
    filter(CYTOKINE %in% cyts, DAY %in% days, ACTARMCD %in% acts) %>%
    mutate(CYTOKINE = factor(CYTOKINE, levels = cytokineLevels)) %>%
    mutate(ACTARMCD = factor(ACTARMCD, levels = vaccineLevels))
  # irritating but ifelse and case_when dont allow RHS to be different in mutate
  if(plottype != 'Lines') dataFiltered <- mutate(dataFiltered,DAY = as.factor(DAY))

  cdp$data <- dataFiltered
  cdp$plot <- ggplotCytokinesForTreatmentDay(dataFiltered,wrap, plottype)
  
}

ggplotCytokinesForTreatmentDay <-
  function(data2plot, wrap, plottype) {
    if (is.null(data2plot) || nrow(data2plot) == 0)
      return(NULL)
    
    grbs <- lapply(unique(data2plot[["ACTARMCD"]]), function(actarmcd) {
      fdata1 <- filter(data2plot, ACTARMCD == actarmcd)
        plots <- lapply(unique(data2plot[["CYTOKINE"]]), function(cyto) {
          fdata2 = filter(fdata1, CYTOKINE == cyto)

          plot <-
            ggplot(
              fdata2,
              mapping = aes(x = DAY, y = VALUE)
            ) +
            themeBase() +
            scale_color_manual(values = cytokineColours) +
            scale_fill_manual(values = cytokineColours)
          
          switch (
            plottype,
            'Violin' = {
              plot <-
                plot + geom_violin(mapping = aes(colour = CYTOKINE, fill = CYTOKINE),
                                   alpha = 0.4)
            },
            'Boxplot' = {
              plot <-
                plot + geom_boxplot(
                  mapping = aes(
                    colour = CYTOKINE,
                    fill = CYTOKINE,
                    group = DAY
                  ),
                  alpha = 0.4
                )
            },
            'Lines' = {
              daybreaks <- unique(data2plot$DAY)
              plot <- plot +
                stat_summary(
                  geom = 'ribbon',
                  fun.data = "mean_se",
                  mapping = aes(fill = CYTOKINE, group = CYTOKINE),
                  alpha = 0.4
                ) +
                stat_summary(
                  geom = 'line',
                  fun.y = "mean",
                  mapping = aes(colour = CYTOKINE, group = CYTOKINE)
                ) +
                scale_x_continuous(breaks = daybreaks) +
                geom_vline(
                  xintercept = daybreaks,
                  color = 'grey80',
                  alpha = 0.5,
                  show.legend = FALSE
                )
            }
          )
          
          # plot <- plot + facet_wrap( ~ CYTOKINE, scales = 'free_y', ncol = 4)
          
          plot <- plot + ggtitle(as.character(cyto))
          
          # switch (wrap,
          #         'TC' = {plot <- plot+facet_wrap(ACTARMCD ~ CYTOKINE, scales = 'free_y', ncol = 4)},
          #         'CT' = {plot <- plot+facet_wrap(CYTOKINE ~ ACTARMCD, scales = 'free_y', ncol = 4)}
          # )
          
          return(plot)
        })

        return(arrangeGrob(grobs = plots,
                            ncol = 4,
                            nrow = ceiling(length(unique(data2plot[["CYTOKINE"]]))/4),
                            top = as.character(actarmcd)))
      })

    return(marrangeGrob(
      grobs = grbs,
      ncol = 1,
      nrow = length(unique(data2plot[["ACTARMCD"]])),
      top = NULL
    ))
  }