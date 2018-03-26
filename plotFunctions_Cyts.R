# Enums ###########
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

# Code ###########
getCytokineMaxMins <- function(data2Max,fixedy,plottype) {
  if(fixedy == FALSE) return(NULL)
  if(plottype == 'Lines') {
    maxsmins <- data2Max %>%
      group_by(CYTOKINE) %>%
      summarise(MAX = max(MEAN+SE, na.rm = TRUE),
                MIN = min(MEAN-SE, na.rm = TRUE))
  } else {
    maxsmins <- data2Max %>%
      group_by(CYTOKINE) %>%
      summarise(MAX = max(VALUE, na.rm = TRUE),
                MIN = min(VALUE, na.rm = TRUE))
  }
  # make a named list
  maxs <- set_names(maxsmins$MAX,nm = maxsmins$CYTOKINE)
  mins <- set_names(maxsmins$MIN,nm = maxsmins$CYTOKINE)
  
  return(list(mx = maxs, mn = mins))
}

getCytokinesDataAndPlot <- function(data2plot, cyts, days, acts, wrap, plottype,error,zoom,fixedy,omit0,showN,nCols) {#cdp, 
  if (is.null(data2plot) || nrow(data2plot) == 0) return(list(data = NULL, plot = NULL))
  
  showNotification("Please wait for data table and plot output. This may take a long time if many cytokines ~ vaccines selected…", type = 'message', duration = 10)
  
  dataFiltered <- data2plot %>%
    filter(CYTOKINE %in% cyts, DAY %in% days, ACTARMCD %in% acts)
  
  if(omit0 == TRUE) {
    dataFiltered <- dataFiltered %>%
      filter(is.na(VALUE) == FALSE, VALUE > 0)
  }
  
  dataFiltered <- dataFiltered %>%
    # preserve the order of the selects with levels
    mutate(CYTOKINE = factor(CYTOKINE, levels = unique(cyts))) %>% 
    mutate(ACTARMCD = factor(ACTARMCD, levels = unique(acts)))
  # irritating but ifelse and case_when dont allow RHS to be different in mutate
  if(plottype == 'Lines') {
    dataFiltered <- dataFiltered %>%
      group_by(ACTARMCD,DAY,CYTOKINE) %>%
      summarise(
        MEAN = mean(VALUE, na.rm = TRUE),
        N = n(),
        # N = 1 or 0 introduces NAs for SE which replicate into max/mins
        SE = case_when(N>1 ~ sd(VALUE, na.rm = TRUE)/sqrt(N), TRUE ~ 0))

  } else {
    dataFiltered <- dataFiltered %>%
      mutate(DAY = as.factor(DAY)) %>%
      arrange(ACTARMCD,DAY,CYTOKINE)
  }

  return(list(data = dataFiltered, 
              plot =  ggplotCytokinesForTreatmentDay(dataFiltered,wrap, plottype,error,zoom,getCytokineMaxMins(dataFiltered,fixedy,plottype),showN,nCols)))

}

ggplotCytokinesForTreatmentDay <-
  function(data2plot, wrap, plottype,error,zoom,yMaxMins,showN,nCols) {
    if (is.null(data2plot) || nrow(data2plot) == 0)
      return(NULL)
    
    switch (wrap,
            'TC' = {v1 <- "ACTARMCD"; v2 <- "CYTOKINE"},
            'CT' = {v2 <- "ACTARMCD"; v1 <- "CYTOKINE"}
    )

    grbs <- lapply(unique(data2plot[[v1]]), function(vv1) {
      # if I understood enquo, quo and !! I could do this in one go without switches
      fdata1 <- 
      switch (wrap,
              "TC" = {filter(data2plot, ACTARMCD == vv1)},
              "CT" = {filter(data2plot, CYTOKINE == vv1)}
      )

      plots <- lapply(unique(data2plot[[v2]]), function(vv2) {
        fdata2 <- 
          switch (wrap,
                  "CT" = {filter(fdata1, ACTARMCD == vv2)},
                  "TC" = {filter(fdata1, CYTOKINE == vv2)}
          )
        
          plot <-
            ggplot(
              fdata2,
              mapping = aes(x = DAY)
            ) +
            themeBase() +
            scale_color_manual(values = cytokineColours, guide = 'none') +
            scale_fill_manual(values = cytokineColours, guide = 'none')
          
          # NAs sneak in and crash when we combine some options and omit 0
          if(is.null(yMaxMins) == FALSE && is.na(fdata2$CYTOKINE[1]) == FALSE) {
            # only 1 cytokine by now
            just <- 'outward'
            mx <- (yMaxMins$mx[[as.character(fdata2$CYTOKINE[1])]])
            mn <- (yMaxMins$mn[[as.character(fdata2$CYTOKINE[1])]])
            plot <- plot + scale_y_continuous(limits = c(mn,mx))
            labY <- mx
          } else {
            labY <- Inf
            just <- 'inward'
          }
          
          switch (
            plottype,
            'Violin' = {
              plot <-
                plot + geom_violin(mapping = aes(y = VALUE, colour = CYTOKINE, fill = CYTOKINE),
                                   alpha = 0.4)
              if(zoom == TRUE) {plot <- plot + coord_cartesian(ylim = quantile(fdata2$VALUE, c(0.08, 0.92),na.rm = TRUE))}
            },
            'Boxplot' = {
              plot <-
                plot + geom_boxplot(
                  mapping = aes(
                    y = VALUE, 
                    colour = CYTOKINE,
                    fill = CYTOKINE,
                    group = DAY
                  ),
                  alpha = 0.4
                )
              if(zoom == TRUE) {plot <- plot + coord_cartesian(ylim = quantile(fdata2$VALUE, c(0.1, 0.9),na.rm = TRUE))}
              
            },
            'Lines' = {
              daybreaks <- unique(fdata2$DAY)

              # skip gridlines if errorbars
              plot <- 
                switch (error,
                  "ribbon" = {
                    plot +
                    geom_vline(xintercept = daybreaks,color = 'grey80',alpha = 0.5,show.legend = FALSE) +
                    geom_ribbon(
                    mapping = aes(fill = CYTOKINE, group = CYTOKINE, ymin = MEAN-SE, ymax = MEAN+SE),
                    alpha = 0.4)},
                  "errorbar" = {
                    plot +
                    geom_errorbar(
                    mapping = aes(color = CYTOKINE, group = CYTOKINE, ymin = MEAN-SE, ymax = MEAN+SE), width = 0.1, size = 0.2)},
                  # default
                  {plot + geom_vline(xintercept = daybreaks,color = 'grey80',alpha = 0.5,show.legend = FALSE)}
                )
              
              plot <- plot +
                geom_line(
                  mapping = aes(y = MEAN, colour = CYTOKINE, group = CYTOKINE)
                ) +
                scale_x_continuous(breaks = daybreaks)
              
              if(showN == TRUE) {
                plot <- plot + geom_text(mapping = aes(label = N), y = labY, hjust = 0.5, vjust = just)
              }
            }
          )
          
          plot <- plot + ggtitle(as.character(vv2))
          

          return(plot)
        })
      nplots <- length(unique(data2plot[[v2]]))
      ncolsperrow = min(nplots,nCols)
      pg <- plot_grid(plotlist = plots,
                    align = 'hv',
                    ncol = ncolsperrow,
                    nrow = ceiling(nplots/ncolsperrow))
        return(arrangeGrob(pg,
                            ncol = 1,
                            nrow = 1,
                            top = as.character(vv1)))
      })

    return(marrangeGrob(
      grobs = grbs,
      ncol = 1,
      nrow = length(unique(data2plot[[v1]])),
      top = NULL
    ))
  }