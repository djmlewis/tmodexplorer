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

themeCyto <- themeBase() + 
  # tweak the sub plots titles
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14))
  
# Code ###########
getCytokineMaxMins <- function(data2Max,fixedy,plottype,error) {
  if(fixedy == FALSE) return(NULL)
  maxsmins <- data2Max
    # dont add SE if we are not plotting it
  if(plottype == 'Lines') {
    if(error == 'none') {maxsmins$SE <- 0}
    maxsmins <- maxsmins %>%
      group_by(CYTOKINE) %>%
      summarise(MAX = max(MEAN+SE, na.rm = TRUE),
                MIN = min(MEAN-SE, na.rm = TRUE))
  } else {
    maxsmins <- maxsmins %>%
      group_by(CYTOKINE) %>%
      summarise(MAX = max(VALUE, na.rm = TRUE),
                MIN = min(VALUE, na.rm = TRUE))
  }
  # make a named list
  maxs <- set_names(maxsmins$MAX,nm = maxsmins$CYTOKINE)
  mins <- set_names(maxsmins$MIN,nm = maxsmins$CYTOKINE)
  
  return(list(mx = maxs, mn = mins))
}

getCytokinesDataAndPlot <- function(data2plot, cyts, days, acts, wrap, plottype,error,zoom,fixedy,omit0,showN,nCols,FIraw, showPoints,Ytrans,show1) { 
  if (is.null(data2plot) || nrow(data2plot) == 0) return(list(data = NULL, plot = NULL))
  
  showNotification("Please wait for data table and plot output. This may take a long time if many cytokines ~ vaccines selected…", type = 'message', duration = 10)
  
  dataFiltered <- data2plot %>%
    filter(CYTOKINE %in% cyts, DAY %in% days, ACTARMCD %in% acts)%>%
    # preserve the order of the selects with levels
    mutate(CYTOKINE = factor(CYTOKINE, levels = unique(cyts))) %>% 
    mutate(ACTARMCD = factor(ACTARMCD, levels = unique(acts)))
  if(plottype != 'Lines') {
    dataFiltered <- dataFiltered %>%
      mutate(DAY = factor(DAY, levels = unique(days))) %>%
      arrange(ACTARMCD,DAY,CYTOKINE)
  }
  # calc groupsize perday here before we do anything else
  dataGroups <- dataFiltered %>%
    group_by(ACTARMCD,DAY,CYTOKINE) %>%
    summarise(GROUP = n()) 
  
  if(omit0 == TRUE) {
    dataFiltered <- dataFiltered %>%
      filter(VALUE > 0)
  }
  
  if(Ytrans != 'identity') {
    dataFiltered$VALUE<- 
      switch (Ytrans,
              log = log(dataFiltered$VALUE),
              log10 = log10(dataFiltered$VALUE),
              log2 = log2(dataFiltered$VALUE),
              log1p = log1p(dataFiltered$VALUE)
      )
  }
  
  if(plottype == 'Lines') {
    dataFiltered <- dataFiltered %>%
      group_by(ACTARMCD,DAY,CYTOKINE) %>%
      summarise(
        MEAN = mean(VALUE, na.rm = TRUE),
        N = sum(!is.na(VALUE)),
        # N = 1 or 0 introduces NAs for SE which replicate into max/mins
        SE = case_when(N>1 ~ sd(VALUE, na.rm = TRUE)/sqrt(N), TRUE ~ 0))
    
  }

  
  if(Ytrans != 'identity') {
    dataFiltered <- dataFiltered %>%
      mutate(TRANSFORM = Ytrans)
  }
    
    
  return(list(
                data = dataFiltered,
                datagroups = dataGroups,
                plot =  ggplotCytokinesForTreatmentDay(dataFiltered,dataGroups,wrap, plottype,error,zoom,
                getCytokineMaxMins(dataFiltered,fixedy,plottype,error),showN,nCols,FIraw, showPoints,Ytrans,show1)
              )
         )
}

ylabForTransform <- function(lab,trans) {
  if(trans == 'identity') return(lab)
  return(paste0(trans,'(',lab,')'))
}


nData <- function(data2N,plottype) {
  # Lines already has this info
  if(plottype == 'Lines') return(data2N)
  ndata <- data2N %>%
    group_by(DAY) %>%
    summarise(
      N = sum(!is.na(VALUE))
    )

  return(ndata)
}

ggplotCytokinesForTreatmentDay <-
  function(data2plot, dataGroups,wrap, plottype,error,zoom,yMaxMins,showN,nCols,FIraw,showPoints,Ytrans,show1) {
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
            themeCyto +
            scale_color_manual(values = cytokineColours, guide = 'none') +
            scale_fill_manual(values = cytokineColours, guide = 'none') +
            ylab(ylabForTransform(FIraw,Ytrans))
          
          if(show1 == TRUE) {
            plot <-  plot + geom_hline(yintercept = 1.0, linetype = 2)
          }

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
            plot <- plot + scale_y_continuous()
          }
          # hack for violins and boxplots that zoom Y
          if(plottype != 'Lines' && zoom == TRUE) {
            labY <- median(fdata2$VALUE, na.rm = TRUE)
            just <- 0.5
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
                geom_line(mapping = aes(y = MEAN, colour = CYTOKINE, group = CYTOKINE)) +
                {if(showPoints == TRUE) {geom_point(mapping = aes(y = MEAN, colour = CYTOKINE, group = CYTOKINE), size = 2 ,show.legend = FALSE)}} +
                scale_x_continuous(breaks = daybreaks)
              
            }
          )
          
          if(showN == TRUE) {
            ndata <- nData(fdata2,plottype)
            plot <- plot +  geom_label(data = ndata, alpha = 0,label.size = 0, label.padding = unit(0.5, "lines"), mapping = aes(x = DAY, label = N), hjust = 0.5, y = Inf, vjust = 'inward' ) # y = labY, vjust = just
            gData <- filter(dataGroups,
                            # this avoids having to figure out what vv1 and 2 are
                            as.character(ACTARMCD) == as.character(vv1) | as.character(CYTOKINE) == as.character(vv1), 
                            as.character(ACTARMCD) == as.character(vv2) | as.character(CYTOKINE) == as.character(vv2),
                            # DAY is bloody subtle. Took hours to get right. You have to drop DAY values from dataGroups that may have gone
                            # due to omit0 removing all values for a specific day(s). The side effect of not doing this is that ggplot
                            # re-factors DAY as a character and so you get 0,1,14,2,21.... OMG! Why?
                            DAY %in% unique(fdata2$DAY))
            plot <- plot +  geom_label(data = gData,
              mapping = aes(x = DAY, label = GROUP),alpha = 0,label.size = 0, label.padding = unit(0.25, "lines"), hjust = 0.5, y = -Inf, vjust = 'inward')
          }
          
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
                            top = textGrob(as.character(vv1), gp=gpar(fontface="bold",fontsize=20, padding = 2))
                           ))
      })

    return(marrangeGrob(
      grobs = grbs,
      ncol = 1,
      nrow = length(unique(data2plot[[v1]])),
      top = NULL
    ))
  }