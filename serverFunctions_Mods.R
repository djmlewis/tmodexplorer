modsNameTitle <- function(mods,titles){
  return(paste0(mods,' (',titles,')'))
}

getMaxMinValueFromModulesData <- function(alldata,allcols,medians){
  if(is.null(alldata)) return(c(0,0))
  
  data <- alldata$modulesMeans %>%
    filter(Column %in% allcols)
  
  if(medians) {return(list(Min = floor(min(data$Median, na.rm = TRUE)),Max = ceiling(max(data$Median, na.rm = TRUE))))}
  return(list(Min = floor(min(data$Mean, na.rm = TRUE)),Max = ceiling(max(data$Mean, na.rm = TRUE))))
}


getSortedModulesForVaccDay <- function(data, colN, descend, asMedians) {
  if (dataFrameOK(data)) {
    # data is allData$modulesMeans
    data4VaccDay <- data %>%
      filter(Column == colN)
      if(asMedians) {
        if (descend) {data4VaccDay <- arrange(data4VaccDay, desc(Median))
        } else {data4VaccDay <- arrange(data4VaccDay, Median)}
      } else {
        # Using means 
        if (descend) {data4VaccDay <- arrange(data4VaccDay, desc(Mean))
        } else {data4VaccDay <- arrange(data4VaccDay, Mean)}
      }
    data4VaccDay <- data4VaccDay %>%
      ungroup() %>%
      mutate(Rank = 1:nrow(data4VaccDay)) %>%
      select(Rank, Module, Title, Category, everything())
    
    return(data4VaccDay)
  }
  return(NULL)
}

getModulesForSearch <- function(modslist,search,column){
  if(!dataFrameOK(modslist) || is.null(search)) return(NULL)
  # ignore an empty search
  if(search == "") return(modslist)
  
  if(column == "Module" && grepl(' ',search)) {
    showNotification("Spaces have been stripped", type = 'warning')
    search <- gsub(" ","",search)
  }
  # do the search
  if(grepl(',',search)) {
    # multiple search
    searches <- unlist(strsplit(search,','))
    selMods <- map_dfr(
      searches,
      function(s){
        modslist[grepl(s,modslist[[column]], ignore.case = TRUE),]
      }
    ) %>%
      # avoid duplicate modules
    distinct(Module, .keep_all = TRUE)
  } else {
    selMods <- modslist[grepl(search,modslist[[column]], ignore.case = TRUE),]
  }

  return(selMods)
}

getModulesForRows <- function(mods,start,end){
  if(!dataFrameOK(mods)) return(NULL)
  if(start>end || start>nrow(mods) || end>nrow(mods)){
    showNotification('The rows filter could not be applied. Check From and To match available rows and From is not > To.', type = "warning")
    return(mods)
  }
  selGenes <- mods[start:end,]
  return(selGenes)
}

getModulesForValues <- function(mods,Min,Max,asMedians){
  if(!dataFrameOK(mods) || Min > Max){return(NULL)}
  if(asMedians){
    selGenes <- mods %>%
      filter(between(Median,Min,Max))
  } else {
    selGenes <- mods %>%
      filter(between(Mean,Min,Max))
  }
  return(selGenes)
}

getModulesForTitles <- function(cats,modsdata) {
  if(!dataFrameOK(modsdata) || is.null(cats)) {return(character(0))}
  mods <- map_dfr(cats,function(cat){
    modsdata %>%
    ungroup() %>%
    filter(Title %in% cat) %>%
    mutate(Mods = modsNameTitle(Module,Title)) %>%
    select(Mods)
  })
  return(unique(mods$Mods))
}

lookupModules <- function(mods2find,modmeans,arrangeby) {
  if(is.null(mods2find) || is.null(modmeans)) return(NULL)
  if(grepl(' ',mods2find)) {
    showNotification("Spaces have been stripped", type = 'warning')
    mods2find <- gsub(" ","",mods2find)
  }
  if(grepl(',',mods2find)) {
    # multiple search
    mods2find <- unlist(strsplit(mods2find,','))
  }
  modmeans <- select(modmeans,Module,Title,Category)
  mods <- 
    map_dfr(mods2find,function(m){
      filter(modmeans,grepl(m,Module, ignore.case = TRUE))
    }) %>%
    distinct()
  
  if(arrangeby == 'Module') {
    mods <- mods %>%
      arrange(Module, Title, Category)
  } else if(arrangeby == 'Title') {
    mods <- mods %>%
      arrange(Title, Category, Module) %>%
      select(Title, Category, Module,everything())
  } else {
    mods <- mods %>%
      arrange(Category, Title, Module)  %>%
      select(Category,Title, Module,everything())
  }
    
  
  l <- getModuleMembers(mods$Module)
  if(length(l) == 0) {
    showNotification("No modules found", type = 'error')
    return(NULL)
  }
  
  d <- map_dfr(names(l),function(mod) {
    df <- data.frame(Module = mod, Gene = paste(l[[mod]], collapse = ", "), stringsAsFactors = FALSE)
  })

  mods <- full_join(mods,d,by = "Module")
  
  
  return(mods)
}

