getMaxMinValueFromModulesData <- function(alldata,allcols,medians){
  if(is.null(alldata)) return(c(0,0))
  
  data <- alldata$modulesMeans %>%
    filter(Column %in% allcols)
  
  if(medians) {return(list(Min = floor(min(data$Median, na.rm = TRUE)),Max = ceiling(max(data$Median, na.rm = TRUE))))}
  return(list(Min = floor(min(data$Mean, na.rm = TRUE)),Max = ceiling(max(data$Mean, na.rm = TRUE))))
}


getSortedModulesForVaccDay <- function(data, colN, descend, asMedians) {
  if (!is.null(data)) {
    # data is allData$modulesMeans
    data4VaccDay <- data %>%
      filter(Column == colN)
      # Using means 
      if(asMedians) {
        if (descend) {data4VaccDay <- arrange(data4VaccDay, desc(Mean))
        } else {data4VaccDay <- arrange(data4VaccDay, Mean)}
      } else {
        if (descend) {data4VaccDay <- arrange(data4VaccDay, desc(Median))
        } else {data4VaccDay <- arrange(data4VaccDay, Median)}
      }
    data4VaccDay <- data4VaccDay %>%
      ungroup() %>%
      mutate(Rank = 1:nrow(data4VaccDay)) %>%
      select(Rank, everything())
    
    return(data4VaccDay)
  }
  return(NULL)
}

getModulesForSearch <- function(modslist,search,column){
  if(is.null(modslist) || is.null(search)) return(NULL)
  # ignore an empty search
  if(search == "") return(modslist)
  # do the search
  if(grepl(',',search)) {
    # multiple search
    searches <- unlist(strsplit(search,','))
    selMods <- map_dfr(
      searches,
      function(s){
        modslist[grepl(s,modslist[[column]], ignore.case = TRUE),]
      }
    )
  } else {
    selMods <- modslist[grepl(search,modslist[[column]], ignore.case = TRUE),]
  }
  selMods <- selMods %>%
    # avoid duplicate modules
    distinct(Module, .keep_all = TRUE)
  
  return(selMods)
}

getModulesForRows <- function(mods,start,end){
  if(is.null(mods)) return(NULL)
  if(start>end || start>nrow(mods) || end>nrow(mods)){
    showModalGenericFailure('The rows filter could not be applied. Check From and To match available rows and From is not > To.')
    return(mods)
  }
  selGenes <- mods[start:end,]
  return(selGenes)
}

getModulesForValues <- function(mods,Min,Max,asMedians){
  if(is.null(mods) || Min > Max){return(NULL)}
  if(asMedians){
    selGenes <- mods %>%
      filter(between(Median,Min,Max))
  } else {
    selGenes <- mods %>%
      filter(between(Mean,Min,Max))
  }
  return(selGenes)
}
