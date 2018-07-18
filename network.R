getNetworkEdgelist <- function(data, vaccs_day, numRows, descend) {
  if(is.null(data)) return(NULL)
  
  sortedData <- map_dfr(vaccs_day, function(vd){
    data2Sort <- select(data,Value = one_of(vd),Gene) %>%
      group_by(Gene) %>%
      summarise(
        MeanValue = mean(Value, na.rm = TRUE)
      ) %>%
      ungroup()
    
    if(descend) data2Sort <- arrange(data2Sort,desc(MeanValue))
    else data2Sort <- arrange(data2Sort,MeanValue)
    data2Sort <- data2Sort[1:numRows,]
    
    genes <- data.frame(from = data2Sort[["Gene"]],
                        to = vd, 
                        rank = 1:numRows,
                        revrank = numRows:1,
                        value = data2Sort[["MeanValue"]],
                        stringsAsFactors = FALSE)
    return(genes)
  }) %>%
  # we must arrange because summarise in edgeCount does arrange, whereas unique does not
    arrange(from)%>%
    mutate(
      from = gsub("[_ :]","\n",from),
      to = gsub("[_ :]","\n",to)
    )
  
  return(sortedData)
}
getNetworkEdgeCounts <- function(data2EdgeCount, edgeFilter, edgeCountThreshold,  nodeAlpha = 0.9) {
  if(is.null(data2EdgeCount)) return(NULL)
  
  edgecount <- data2EdgeCount %>%
    group_by(from) %>%
    summarise(edgeN = n())
  
  if(edgeFilter != "a") {
    connected <- switch (edgeFilter,
                         "u" = edgecount[edgecount$edgeN == 1,][["from"]],
                         "c" = edgecount[edgecount$edgeN>1,][["from"]],
                         "v" = edgecount[edgecount$edgeN>edgeCountThreshold,][["from"]],
                         NULL #redundant pull error
    )
    if(length(connected) < 1) return(NULL)
    
    edgecount <- edgecount[edgecount$from %in% connected,]

  }
  
  nodes <- c(unique(data2EdgeCount$from),unique(data2EdgeCount$to))
  
  mypal <- rev(heat.colors(length(unique(data2EdgeCount$to)), alpha = nodeAlpha))
  mypal[1] <- "#FFFFFF"
  edgecount <- edgecount %>%
    mutate(edgecol = mypal[edgeN])
  
  return(edgecount)
}

getNetworkQgraph <- function(data2q, edgesN, netType,lineWidthCol) {
  if(is.null(data2q) || is.null(edgesN)) return(NULL)

  # we may have reduced the nodes by edgecount
  data2q <- data2q[data2q$from %in% edgesN$from,]
  
  nodecolours <- c(edgesN$edgecol,rep('#eaeaea',length(unique(data2q$to))))
  nodeshapes <- c(rep('circle',length(edgesN$edgecol)),rep('square',length(unique(data2q$to))))

  qg <- qgraph(data2q[,c("from","to",lineWidthCol)],
               DoNotPlot = TRUE,
               color = nodecolours,
               shape = nodeshapes,
               edgelist = TRUE, weighted= TRUE,
               arrows = FALSE, posCol = 'black',
               legend = FALSE, 
               layout = netType
  )

  return(qg)
}

