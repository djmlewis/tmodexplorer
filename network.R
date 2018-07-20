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
      mutate(MeanValueRound = round(MeanValue,2))
    
    genes <- data.frame(Gene = data2Sort[["Gene"]],
                        Vaccine.Day = vd, 
                        Rank = 1:numRows,
                        revrank = numRows:1,
                        MeanValue = data2Sort[["MeanValue"]],
                        MeanValueRound = data2Sort[["MeanValueRound"]],
                        stringsAsFactors = FALSE)
    return(genes)
  }) %>%
  # we must arrange because summarise in edgeCount does arrange, whereas unique does not
    arrange(Gene)
  
  vennData <- map(vaccs_day, function(vd){
    filter(sortedData,Vaccine.Day == vd)[["Gene"]]
  })
  names(vennData) <- vaccs_day
  if(length(vennData)>5) {
    showNotification("Only first 5 groups shown in Venn diagram", type = "error")
    vennData <- vennData[1:5]
  }
  return(list(data = sortedData, venndiag = venn.diagram(vennData, NULL, margin = 0.05)))
}

getEdgeMinMax<- function(edgelist,connection) {
  connection <- ifelse(connection == "revrank","Rank","MeanValue")
  return(list(Min = floor(min(edgelist[[connection]],na.rm = TRUE)), Max = ceiling(max(edgelist[[connection]],na.rm = TRUE))))
}

getNetworkEdgeCounts <- function(data2EdgeCount) {
  if(is.null(data2EdgeCount)) return(NULL)
  edgecount <-data2EdgeCount

  edgecount <- edgecount %>%
    group_by(Gene) %>%
    summarise(Connections = n())

  return(edgecount)
}

getNetworkFilteredEdgeCounts <- function(data2EdgeCount, edgeFilter, edgeCountThreshold) {
  if(is.null(data2EdgeCount)) return(NULL)
  edgecount <- data2EdgeCount %>%
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

getGGplotEdgeColourLegend <- function(filteredEdgelist){
  if(is.null(filteredEdgelist)) return(NULL)
  numConnex <- max(filteredEdgelist$Connections,na.rm = TRUE)
  df <- data.frame(X = 1:numConnex, Y = rep(1,numConnex))
  plot <- ggplot(df, mapping = aes(x = X, y = Y)) +
    geom_text(aes(label = X))
  print(plot)
  return(plot)
}

getNetworkQgraph <- function(data2q, edgeCountData, netType,edgeWidthVar,showLineLabels,nodeAlpha,
          edgeValueThresholdLo,edgeValueThresholdHi,applyEdgeValueThreshold) {
  if(is.null(data2q) || is.null(edgeCountData)) return(NULL)
  # we may have reduced the nodes by edgecount
  data2q <- data2q[data2q$Gene %in% edgeCountData$Gene,]
  
  # now apply the edge value to remaining edges
  if(applyEdgeValueThreshold == TRUE) {
    data2q <- switch(edgeWidthVar,
                        "revrank" = filter(data2q,between(Rank,edgeValueThresholdLo,edgeValueThresholdHi)),
                        filter(data2q,between(MeanValue,edgeValueThresholdLo,edgeValueThresholdHi)))
    # if no rows left then abort
    if(nrow(data2q)<1) return(NULL)
  
    # now reduce edgeCountData to match the genes remaining in data2q
    edgeCountData <- edgeCountData[edgeCountData$Gene %in% data2q$Gene,]
    
  }
  
  numVaccNodes <- length(unique(data2q$Vaccine.Day))
  # numGeneNodes <- length(data2q$Gene)
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
  if(showLineLabels == TRUE) linelabels <- switch(edgeWidthVar,'revrank' = data2q$Rank, data2q$MeanValueRound)
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

