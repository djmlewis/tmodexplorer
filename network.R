prettifyName <- function(name,style){
  switch (style,
    'nl' = gsub("[_ :]","\n",name),
    '(_)' =paste0(gsub("_"," (",name),')'),
    name
  )
}

vaccColoursForNames <- function(vaccnames, withDays = FALSE,namesNotColours = FALSE){
  if(withDays) vaccnames <- str_split(vaccnames,'_',simplify = TRUE)[,1]
  if(namesNotColours) return(vaccnames)
  vaccCols <- map_chr(vaccnames,~vaccineColours[[.]])
  return(vaccCols)
}

eulerFromVaccGenesList <- function(vennData){
  if(is.null(vennData) || length(vennData)==0) return(NULL)
  vacCols <- vaccColoursForNames(names(vennData),TRUE)
  names(vennData) <- prettifyName(names(vennData),"(_)")
  plot(euler(vennData),
     edges = list(col = vacCols, lwd = 4),
     fills = list(fill = vacCols, alpha = 0.1),
     quantities = list(col = 'black', fontsize = 32),
     labels = list(fontsize = 24)
  )
}

venDiagramFromVaccGenesList <- function(vennData){
  if(is.null(vennData) || length(vennData)==0) return(NULL)
  if(length(vennData)>5) {
    showNotification("Only first 5 groups shown in Venn diagram", type = "error")
    vennData <- vennData[1:5]
  }
  vacNames <- names(vennData)
  vacCols <- vaccColoursForNames(vacNames,TRUE)
  names(vennData) <- prettifyName(vacNames,"(_)")
  
  vd <- venn.diagram(
    vennData, 
    filename = NULL, 
    col = vacCols,
    fill = vacCols,
    cat.cex = rep(2,length(vennData)),
    cat.fontfamily = rep('sans',length(vennData)),
    cat.fontface = rep(2.2,length(vennData)),
    cat.col = vacCols,
    cex = rep(2,(2^length(vennData))-1),
    fontfamily = rep('sans',(2^length(vennData))-1),
    lwd = 4,
    alpha = 0.1,
    margin = 0.16)
  
  res <- file.remove(list.files(pattern = "^VennDiagram.*log$"))
  return(grid.draw(vd))
}

Intersect_venn <- function (x) {  
  # Multiple set version of intersect
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect_venn(x[-1]))
  }
}

Union_venn <- function (x) {  
  # Multiple set version of union
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union_venn(x[-1]))
  }
}

Setdiff_venn <- function (x, y) {
  # Remove the union of the y's from the common x's. 
  # x and y are lists of characters.
  xx <- Intersect_venn(x)
  yy <- Union_venn(y)
  setdiff(xx, yy)
}

geneIntersectsFromVaccGenesList_Internal <- function(vennData){
  if(is.null(vennData) || length(vennData)==0) return(NULL)
  
  combs <-  unlist(lapply(1:length(vennData), function(j) combn(names(vennData), j, simplify = FALSE)), recursive = FALSE)
  names(combs) <- sapply(combs, function(i) paste0(i, collapse = " : "))
  elements <-  lapply(combs, function(i) Setdiff_venn(vennData[i], vennData[setdiff(names(vennData), i)]))
  # n.elements <- sapply(elements, length)
  elementsDF <- map_dfr(names(elements),function(name){
    data_frame(Group = name,Genes = paste0(elements[[name]],collapse = ', '))
  })
  
  return(elementsDF)
  
}



upsetrFromVaccGenesList <- function(vennData){
  if(is.null(vennData) || length(vennData)<2) return(NULL)
  names(vennData) <- prettifyName(names(vennData),"(_)")
  return(upset(fromList(vennData),
          nsets = length(vennData),
          main.bar.color = '#728f17',
          sets.bar.color = '#eab945',
          matrix.color = '#4d600f',
          point.size = 4,
          nintersects = NA,
          scale.intersections = 'identity',
          shade.color = '#f8ffeb',
          text.scale = 3
  ))
}

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
  
  vennData <- setNames(map(vaccs_day, function(vd){
    filter(sortedData,Vaccine.Day == vd)[["Gene"]]
  }),vaccs_day)
  # names(vennData) <-vaccs_day# prettifyName(vaccs_day,'(_)')

  return(list(data = sortedData, vennData = vennData))
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
  vaccnames <- vaccColoursForNames(unique(data2q$Vaccine.Day),TRUE,TRUE)
  vaccols <- vaccColoursForNames(vaccnames)
  nodecolours <- c(edgeCountData$edgecol,rep('white',numVaccNodes))
  nodebordercolours <- c(rep('black',numGeneNodes),vaccols)
  nodeshapes <- c(rep('circle',numGeneNodes),rep('square',numVaccNodes))
  if(showLineLabels == TRUE) linelabels <- switch(edgeWidthVar,'revrank' = data2q$Rank, data2q$MeanValueRound)
  else linelabels <- FALSE
  
  #fix the names
  data2q <- data2q %>%
    mutate(
      Vaccine.Day = prettifyName(Vaccine.Day,'nl'),
      Gene = prettifyName(Gene,'nl')
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

