getNetworkEdgelist <- function(data, vaccs_day, numRows, descend) {
  if(is.null(data)) return(NULL)
  
  sortedData <- map_dfr(vaccs_day, function(vd){
    data2Sort <- select(data,MeanValue = one_of(vd),Gene) %>%
      group_by(Gene) %>%
      summarise(
        MeanValue = mean(MeanValue, na.rm = TRUE)
      ) %>%
      ungroup()
    
    if(descend) data2Sort <- arrange(data2Sort,desc(MeanValue))
    else data2Sort <- arrange(data2Sort,MeanValue)
    data2Sort <- data2Sort[1:numRows,] %>%
      mutate(MeanValue = round(MeanValue,2))
    
    genes <- data.frame(Gene = data2Sort[["Gene"]],
                        Vaccine.Day = vd, 
                        Rank = 1:numRows,
                        revrank = numRows:1,
                        MeanValue = data2Sort[["MeanValue"]],
                        stringsAsFactors = FALSE)
    return(genes)
  }) %>%
  # we must arrange because summarise in edgeCount does arrange, whereas unique does not
    arrange(Gene)
  
  return(sortedData)
}
getNetworkEdgeCounts <- function(data2EdgeCount, edgeFilter, edgeCountThreshold,edgeValueThresholdLo,edgeValueThresholdHi,applyEdgeValueThreshold,edgeWidthVar) {
  if(is.null(data2EdgeCount)) return(NULL)
  edgecount <-data2EdgeCount
  if(applyEdgeValueThreshold == TRUE)  edgecount <- edgecount %>% 
      filter_at(vars(switch(edgeWidthVar,"revrank" = "Rank","MeanValue")),all_vars(between(.,edgeValueThresholdLo,edgeValueThresholdHi)))
  if(nrow(edgecount)<1) return(NULL)
  
  edgecount <- edgecount %>%
    group_by(Gene) %>%
    summarise(Connections = n())
  
  if(edgeFilter != "a") {
    connected <- switch (edgeFilter,
                         "u" = edgecount[edgecount$Connections == 1,][["Gene"]],
                         "c" = edgecount[edgecount$Connections>1,][["Gene"]],
                         "v" = edgecount[edgecount$Connections>edgeCountThreshold,][["Gene"]],
                         NULL #redundant - post error
    )
    if(length(connected) < 1) return(NULL)
    
    edgecount <- edgecount[edgecount$Gene %in% connected,]

  }

  return(edgecount)
}

getNetworkQgraph <- function(data2q, edgeCountData, netType,edgeWidthVar,showLineLabels,nodeAlpha) {
  if(is.null(data2q) || is.null(edgeCountData)) return(NULL)
  # we may have reduced the nodes by edgecount
  data2q <- data2q[data2q$Gene %in% edgeCountData$Gene,]
  numVaccNodes <- length(unique(data2q$Vaccine.Day))
  numGeneNodes <- nrow(edgeCountData)
  
  mypal <- rev(heat.colors(max(edgeCountData$Connections, na.rm = TRUE), alpha = nodeAlpha))
  mypal[1] <- "#FFFFFF"
  edgeCountData <- edgeCountData %>%
    mutate(edgecol = mypal[Connections])
  # add the vaccine colours the others
  vaccnames <- str_split(unique(data2q$Vaccine.Day),'_',simplify = TRUE)[,1]
  vaccols <- map_chr(vaccnames,~vaccineColours[[.]])
  nodecolours <- c(edgeCountData$edgecol,rep('white',numVaccNodes))
  nodebordercolours <- c(rep('black',numGeneNodes),vaccols)
  nodeshapes <- c(rep('circle',numGeneNodes),rep('square',numVaccNodes))
  if(showLineLabels == TRUE) linelabels <- switch(edgeWidthVar,'revrank' = data2q$Rank, data2q[[edgeWidthVar]])
  else linelabels <- FALSE
  
  #fix the names
  data2q <- data2q %>%
    mutate(
      Vaccine.Day = gsub("[_ :]","\n",Vaccine.Day),
      Gene = gsub("[_ :]","\n",Gene)
    )
  qg <- qgraph(data2q[,c("Gene","Vaccine.Day",edgeWidthVar)],
               DoNotPlot = TRUE,
               color = nodecolours,
               shape = nodeshapes,
               border.color = nodebordercolours, border.width = c(rep(0.75,numGeneNodes),rep(5,numVaccNodes)),
               edge.labels = linelabels,
               edgelist = TRUE, weighted= TRUE,
               arrows = FALSE, posCol = 'black',
               legend = FALSE, 
               layout = netType
  )

  return(qg)
}

