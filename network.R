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
  vaccCols <- map_chr(vaccnames,function(vn) {
    if(vn %in% names(vaccineColours)) return(vaccineColours[[vn]])
    else {
      showNotification(paste("Vaccine Color not found for",vn), type = 'error', duration = 3)
      return("gray")
    }
    })
  return(vaccCols)
}

eulerFromVaccGenesList <- function(vennData,shapes){
  if(is.null(vennData) || length(vennData)==0) return(NULL)
  vacCols <- vaccColoursForNames(names(vennData),TRUE)
  names(vennData) <- prettifyName(names(vennData),"(_)")
  e <- euler(vennData, shape = shapes)
  p <- plot(e,
     edges = list(col = vacCols, lwd = 4),
     fills = list(fill = vacCols, alpha = 0.1),
     quantities = list(col = 'black', fontsize = 32),
     labels = list(fontsize = 24)
  )
  return(p)
}

venDiagramFromVaccGenesList <- function(vennData){
  if(is.null(vennData) || length(vennData)==0) return(NULL)
  if(length(vennData)>5) {
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
  return(vd)
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

geneIntersectsFromVaccGenesList <- function(vennData){
  if(is.null(vennData) || length(vennData)==0) return(NULL)
  names(vennData) <- prettifyName(names(vennData),"(_)")
  
  combs <-  unlist(lapply(1:length(vennData), function(j) combn(names(vennData), j, simplify = FALSE)), recursive = FALSE)
  names(combs) <- sapply(combs, function(i) paste0(i, collapse = " : "))
  elements <-  lapply(combs, function(i) Setdiff_venn(vennData[i], vennData[setdiff(names(vennData), i)]))
  # n.elements <- sapply(elements, length)
  elementsDF <- map_dfr(names(elements),function(name){
    tibble(Set = name,Genes = paste0(elements[[name]],collapse = ', '))
  }) %>%
    filter(nchar(Genes)>0)
  
  return(elementsDF)
  
}

upsetrFromVaccGenesList <- function(vennData,orderby, emptyintersections){
  if(is.null(vennData)) return(NULL)
  if(length(vennData)<2) {
    return(grid.text("At least two sets are required for UpSetR plots"))
  }
  
  if(emptyintersections == TRUE) emptyintersections <- length(vennData) else emptyintersections <- NULL
  names(vennData) <- prettifyName(names(vennData),"(_)")
  ups <- upset(fromList(vennData),
               nsets = length(vennData),
               empty.intersections = emptyintersections,
               order.by = switch(orderby, 'f' = "freq", 'd' = "degree", 'fd' = c("freq","degree"), 'df' = c("degree","freq")),
               main.bar.color = '#728f17',
               sets.bar.color = '#eab945',
               matrix.color = '#4d600f',
               point.size = 4,
               nintersects = NA,
               scale.intersections = 'identity',
               shade.color = '#f8ffeb',
               text.scale = 3
  )
  return(ups)
}

upSetRPNG <- function(upsetrdata,orderby, emptyintersections, file, h, w) {
  if (is.null(upsetrdata) || length(upsetrdata)<2)  {
    png(file)
    grid.newpage()
    grid.text("At least two sets are required for UpSetR plots")
    dev.off()
  } else {
    png(
      file,
      height = h * 300 / 72,
      width = w * 300 / 72,
      res = 300,
      units = 'px',
      bg = "white"
    )
    upsetrFromVaccGenesList(upsetrdata,orderby, emptyintersections)
    dev.off()
  }
}


getNetworkEdgeListAndCount <- function(data, vaccs_day, numRows, descend) {
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
  
  edgecount <- sortedData %>%
    group_by(Gene) %>%
    summarise(Connections = n())

  return(list(edgeList = sortedData, edgeCount = edgecount))
}

getVennVaccGenesList <- function(sortedData, vaccs_day) {
  if(is.null(sortedData)) return(NULL)
  vaxInData <- intersect(vaccs_day,unique(sortedData$Vaccine.Day))
  if(length(vaxInData) == 0) return(NULL)
  vennData <- setNames(map(vaxInData, function(vd){
    filter(sortedData,Vaccine.Day == vd)[["Gene"]]
  }),vaxInData)
  return(vennData)
}

getEdgeMinMax<- function(edgelist,connection) {
  connection <- ifelse(connection == "revrank","Rank","MeanValue")
  return(list(Min = floor(min(edgelist[[connection]],na.rm = TRUE)), Max = ceiling(max(edgelist[[connection]],na.rm = TRUE))))
}


getFilteredEdgeListAndEdgeCounts <- function(data2q, edgecount, edgeFilter, edgeCountThreshold,
                                             edgeWidthVar,edgeValueThresholdLo,edgeValueThresholdHi,applyEdgeValueThreshold) {
  if(is.null(data2q) || is.null(edgecount)) return(list(edgeList = NULL, edgeCount = NULL))
  
  # first reduce the edgecount by unique etc
  if(edgeFilter != "a") {
    connected <- switch (edgeFilter,
                         "u" = edgecount[edgecount$Connections == 1,][["Gene"]],
                         "c" = edgecount[edgecount$Connections>1,][["Gene"]],
                         "v" = edgecount[edgecount$Connections>edgeCountThreshold,][["Gene"]],
                         NULL #redundant - post error
    )
    edgecount <- edgecount[edgecount$Gene %in% connected,]
  }
  
  # we may have reduced the nodes by edgecount
  data2q <- data2q[data2q$Gene %in% edgecount$Gene,]
  
  # now apply the edge value to remaining edges
  if(applyEdgeValueThreshold == TRUE) {
    data2q <- switch(edgeWidthVar,
                     "revrank" = filter(data2q,between(Rank,edgeValueThresholdLo,edgeValueThresholdHi)),
                     filter(data2q,between(MeanValue,edgeValueThresholdLo,edgeValueThresholdHi)))
    # if no rows left then abort
    if(nrow(data2q)<1) return(NULL)
    
    # now reduce edgeCountData to match the genes remaining in data2q
    edgecount <- edgecount %>%
      filter(Gene %in% data2q$Gen)
  }
  
  return(list(edgeList = data2q, edgeCount = edgecount))
}

getNetworkQgraph <- function(edgeListAndCount,netType,edgeWidthVar,showLineLabels,nodeAlpha,showLegend) {
  if(is.null(edgeListAndCount)) return(NULL)
  data2q <- edgeListAndCount[['edgeList']]
  edgeCountData <- edgeListAndCount[['edgeCount']]
  if(is.null(data2q) || is.null(edgeCountData) || nrow(data2q) == 0 || nrow(edgeCountData) == 0) return(NULL)

  numVaccNodes <- length(unique(data2q$Vaccine.Day))
  # numGeneNodes <- length(data2q$Gene)
  numGeneNodes <- nrow(edgeCountData)

  mypal <- c(brewer.pal(9,"YlOrRd"),rev(brewer.pal(7,"BuPu")))
  maxedgeCount <- max(edgeCountData$Connections, na.rm = TRUE)
  if(maxedgeCount>16) {
    mypal <- rev(heat.colors(maxedgeCount, alpha = nodeAlpha))
  }
  mypal[1] <- "#FFFFFF"
  edgeCountData <- edgeCountData %>%
    mutate(edgecol = mypal[Connections])
  # add the vaccine colours the others
  vaccnames <- vaccColoursForNames(unique(data2q$Vaccine.Day),TRUE,TRUE)
  vaccols <- vaccColoursForNames(vaccnames)
  vax <- unique(vaccnames)
  vaxcols <- vaccColoursForNames(vax)
  if(showLegend) {
    df <- as_tibble(list(vax = vax, x = factor(vax, levels = vax),  y = factor(vax, levels = vax), colrs = vax))
    plt <- ggplot(data = df, mapping = aes(x = x, y = y, color = vax)) +
      theme_cowplot() +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        # legend.position = 'top',
        legend.direction = 'horizontal'
      ) +
      scale_color_manual(values = vaxcols) +
      geom_point(shape = 15, size = 6)
    lg <- ggpubr::as_ggplot(ggpubr::get_legend(plt))
  } else {
    lg <- NULL
  }
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

  return(list(qgraph = qg, plt = lg))
}

