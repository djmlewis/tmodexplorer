###### GLOBALS for all sessions

load('tmod.rda')


###### Functions
extractColumnNames <- function(cnams) {
  colData <-
    data_frame(COLNAMES = grep('_', cnams, value = TRUE)) %>%
    separate(
      COLNAMES,
      into = c('ACTARMCD', 'VACCDY'),
      sep = '_',
      convert = TRUE
    )
  return(list(actarmcds = as.factor(unique(colData$ACTARMCD)),
              vaccdys = unique(sort(colData$VACCDY))))
}

getMaxMinValueFromData <- function(alldata,allcols){
  if(is.null(alldata)) return(c(0,0))
  data <- alldata %>%
    select(one_of(allcols))
  if(ncol(data)>0) {
    return(list(Min = floor(min(data, na.rm = TRUE)),Max = ceiling(max(data, na.rm = TRUE))))
  } else {
    return(list(Min = 0, Max = 0))
  }
}


showModalFileLoadFailure <- function(extraMessage){
  showModal(
    modalDialog(
      paste0("Unable to load data. Ensure the necessary files with correct names are selected",extraMessage),
      title = "Data Load",
      easyClose = TRUE
    )
  )
}

showModalGenericFailure <- function(message){
  showModal(
    modalDialog(
      message,
      title = "Error",
      easyClose = TRUE
    )
  )
}


getNewData <- function(allData, folderNme) {
  folderpath <- paste0('datafiles/', folderNme) #file.choose()
  annotPath <- paste0(folderpath, '/', 'annotation.rds')
  dataPath <- paste0(folderpath, '/', 'data.rds')
  modPath <- paste0(folderpath, '/', 'modules.rds')
  modmeanPath <- paste0(folderpath, '/', 'modulesMeans.rds')
  
  if (file.exists(dataPath) && file.exists(annotPath)) {
    annotation <- read_rds(annotPath) %>%
      select(Probe = X1, Gene = GeneName, Description)
    
    allData$data <- read_rds(dataPath) %>%
      rename(Probe = X1) %>%
      full_join(annotation, by = 'Probe')
    
    allData$colNames <-
      names(allData$data)[grepl('_', names(allData$data))]
    
    # folderpath has stripped the filename with dirname, so strip up to last directory with basename
    allData$folder <- basename(folderpath)
    allData$folderpath <- folderpath
    
    # try for modules
    if (file.exists(modPath) && file.exists(modmeanPath)) {
      allData$modules <- read_rds(modPath)
      allData$modulesMeans <- read_rds(modmeanPath)
    }
    
    return(TRUE)
  } else {showModalFileLoadFailure('')}
  return(FALSE)
}

loadUploadedData <- function(allData, infiles,fileName) {
  if(!is.null(infiles) && nrow(infiles) == 2 && nchar(fileName) > 0){
    annotPath <- infiles[infiles$name == 'annotation.rds',][['datapath']]
    dataPath <- infiles[infiles$name == 'data.rds',][['datapath']]
    if(!is.null(annotPath)&& !is.null(dataPath)) {
      annotation <- read_rds(annotPath) %>%
        select(Probe = X1, Gene = GeneName, Description)
      
      allData$data <- read_rds(dataPath) %>%
        rename(Probe = X1) %>%
        full_join(annotation, by = 'Probe')
      
      allData$colNames <-
        names(allData$data)[grepl('_', names(allData$data))]
      
      # folderpath has stripped the filename with dirname, so strip up to last directory with basename
      allData$folder <- fileName
      allData$folderpath <- NULL
      return(TRUE)
    }
  }
  showModalFileLoadFailure(' and ensure an Upload File Name is entered in the box')
  return(FALSE)
}


getSortedGenesForVaccDay <- function(data, colN, descend, asGenes) {
  # protect from not matching colN
  if (!is.null(data) && colN %in% names(data)) {
      if (asGenes == TRUE) {
        data4VaccDay <- data %>%
          # matches will find substrings so force it to match the whole string against colN
          select(Value = matches(paste0('^', colN, '$')), Gene, Description,Probe) %>%
          # Description is missing from asGenes as it is probe-specific
          group_by(Gene) %>%
          summarise(
            SD = sd(Value, na.rm = TRUE),
            Value = mean(Value, na.rm = TRUE),
            N = n()) %>%
          ungroup() %>%
          select(Value,SD,N,Gene)
      } else {
        data4VaccDay <- data %>%
          # matches will find substrings so force it to match the whole string against colN
          # Description is available
          select(Value = matches(paste0('^', colN, '$')), Gene, Description,Probe)
      }

      if (descend) {
        data4VaccDay <- arrange(data4VaccDay, desc(Value))
      } else {
        data4VaccDay <- arrange(data4VaccDay, Value)
      }
      
      data4VaccDay <- data4VaccDay %>%
        mutate(Rank = 1:nrow(data4VaccDay)) %>%
        select(Rank, everything())
      
      return(data4VaccDay)
  }
  return(NULL)
}

getTopGenesInSeries <- function(allData, selData,selCols, facet) {
  if(is.null(allData) || length(selCols) == 0) return(NULL)
  
  # asGenes: detect whether it really is as genes based on the selData: if that lacks column Probe then it is
  asGenes <- ('Probe' %in% names(selData) == FALSE)
  if(asGenes == TRUE) {
    seriesData <- allData %>%
      filter(Gene %in% selData$Gene) %>%
      select(Gene,one_of(selCols)) %>%
      group_by(Gene) %>%
      summarise_at(vars(one_of(selCols)),mean, na.rm = TRUE) %>%
      ungroup()
  } else {
    seriesData <- allData %>%
      filter(Probe %in% selData$Probe) %>%
      select(Probe,Gene,one_of(selCols))
  }
  
  seriesData <- seriesData %>%
    gather(key = 'Column', value = 'Value', convert = TRUE, factor_key = FALSE,one_of(selCols))
    
  if(facet == TRUE){
    seriesData <- seriesData %>%
      separate(Column,into = c('Treatment','Column'),sep = '_', convert = TRUE)
  }
  
  return(seriesData)
}

selectedGenesAndModules <- function(selGenes) {
  selMods <- modules4GeneList(selGenes$Gene,selGenes$Rank)
  selModsOnly <- selMods[selMods$Module != '',]
  return(list(genes = selGenes, modules = selMods, modsOnly = selModsOnly))
}

getGenesForRows <- function(genes,start,end){
  if(is.null(genes)) return(NULL)
  if(start>end || start>nrow(genes) || end>nrow(genes)){
    showModalGenericFailure('The rows filter could not be applied. Check From and To match available rows and From is not > To.')
    return(genes)
  }
  selGenes <- genes[start:end,]
  return(selGenes)
  # return(selectedGenesAndModules(selGenes))
}

getGenesForValues <- function(genes,Min,Max){
  if(is.null(genes) || Min > Max){return(NULL)}
  selGenes <- genes %>%
    filter(between(Value,Min,Max))
  return(selGenes)
  # return(selectedGenesAndModules(selGenes))
}

getGenesForSearch <- function(geneslist,search,column){
  if(is.null(geneslist) || is.null(search)) return(NULL)
  # ignore an empty search
  if(search == "") return(geneslist)
  # do the search
  if(grepl(',',search)) {
    # multiple search
    searches <- unlist(strsplit(search,','))
    selGenes <- map_dfr(
      searches,
      function(s){
        geneslist[grepl(s,geneslist[[column]], ignore.case = TRUE),]
      }
    )
  } else {
    selGenes <- geneslist[grepl(search,geneslist[[column]], ignore.case = TRUE),]
  }
  selGenes <- selGenes %>%
    # avoid duplicate Probe
    distinct(Probe, .keep_all = TRUE)
  
  return(selGenes)
}

modules4GeneList <- function(genes2map,genes2mapRanks) {
  # protect from empty data
  if(length(genes2map)>0) {
    geneMods <- map2_dfr(genes2map,genes2mapRanks,function(gene,generank){
      mods <- tmod$GENES2MODULES[[gene]]
      if(length(mods>0)){
        modDescriptions <- map_chr(mods,function(mod){
          return(tmod$MODULES[tmod$MODULES$ID == mod,][['Title']])
        })
        modCats <- map_chr(mods,function(mod){
          return(tmod$MODULES[tmod$MODULES$ID == mod,][['Category']])
        })
        return(data.frame(Rank = generank,Gene = gene,Module = mods,
                          Description = modDescriptions, 
                          Category = modCats, 
                          stringsAsFactors = FALSE))
      }
      return(data.frame(Rank = generank,Gene = gene,Module = c(''),Description = c(''), Category = '', stringsAsFactors = FALSE))
    })
    return(geneMods)
  }
  return(NULL)
}

moduleDescriptionsForGenes <- function(modsOnly){
  if (length(modsOnly) > 0) {
    # remove the row with Selected as module as it has no genes! Its a pseudo module
    modsOnly <- modsOnly %>%
      filter(Module != 'Selected')
    
    return(modsNameTitle(modsOnly[['Module']],modsOnly[['Description']]))
  } 
  return('')
}

getGeneExpressionsInModule <- function(mod, actarmcdDay, allExpressionData,topGenes) {
    # protect from empty data
    if (nchar(mod) > 0 &&
        nchar(actarmcdDay) > 0 &&
        !is.null(allExpressionData) &&
        nrow(allExpressionData) > 0 &&
        !is.null(topGenes))
    {
      # extract just the module name from the name-description
      selModName <- sub(' .*$', '', mod)
      actarmDayExpressionData <- allExpressionData %>%
        select(matches(actarmcdDay), Gene, Probe)
      
      if (ncol(actarmDayExpressionData) > 1) {
        genesInMod <- tmod::getModuleMembers(selModName)[[selModName]]
        genesExpression <- map_dfr(genesInMod, function(gene) {
          exprV <- actarmDayExpressionData %>%
            filter(Gene == gene)
          
          # if gene is in module but not our data we get numeric(0) returned on filter, so swap for NA
          if (nrow(exprV) > 0) {
            df <-
              data.frame(
                Module = selModName,
                Gene = gene,
                Value = exprV[[actarmcdDay]],
                Probe = exprV[['Probe']],
                stringsAsFactors = FALSE
              )
            return(df)
          }
          return(
            data.frame(
              Module = selModName,
              Gene = gene,
              Value = NA,
              Probe = NA,
              stringsAsFactors = FALSE
            )
          )
        })
        selgenes <- topGenes[['Gene']]
        genesExpression <-  genesExpression %>%
          arrange(Value) %>%
          mutate(Gene = factor(Gene, levels = unique(Gene)),
                 Selected = ifelse(Gene %in% selgenes,"►",""))

        return(genesExpression)
      }
    }
    return(NULL)
  }

getExpressionsForModules <- function(topgenesmods, actarmcdDay, allExpressionData, addPseudoModule, filters) {
    
    if (!is.null(topgenesmods) && nrow(topgenesmods[['modsOnly']]) > 0 && nrow(topgenesmods[['genes']]) > 0) {

      actarmDayExpressionData <- allExpressionData %>%
        select(Value = matches(paste0('^', actarmcdDay, '$')), Gene)
      # Extract unique mod names

      modsUnique <- unique(topgenesmods[['modsOnly']][['Module']])

      # get a named list, name is the module and values are gene names
      modsWithGenes <- tmod::getModuleMembers(modsUnique)

      modsExprns <- map_dfr(modsUnique, function(mod) {
        rowsForGenes <- actarmDayExpressionData %>%
          # lookup the genes in the list corresponding to mod name
          filter(Gene %in% modsWithGenes[[mod]]) %>%
          mutate(Module = mod,
                 Description = map_chr(Module, function(m) {
                   tmod$MODULES[tmod$MODULES$ID == mod, ][['Title']]
                 })) %>%
          select(Value, Module, Description)
      })

      if(addPseudoModule == TRUE) {
        # lookup the value of probes in our selected genes for the actamDay which are in topgenesmods[['genes']]
        # just keep Value and give Gene and description a pseudo name, then rowbind
        selecteGeneExpr <- topgenesmods[['genes']][['Value']]
        modsExprns <- modsExprns %>%
          bind_rows(data.frame(Value = selecteGeneExpr, Module = "Selected", Description = filters, stringsAsFactors = FALSE))
      }
      
      modsSummStats <- modsExprns %>%
        group_by(Module, Description) %>%
        summarise(
          Median = median(Value, na.rm = TRUE),
          Mean = mean(Value, na.rm = TRUE),
          SD = sd(Value, na.rm = TRUE),
          N = n()
        ) %>%
        select(Module, Description, everything()) %>%
        arrange(desc(Median)) %>%
        ungroup() %>%
        as.data.frame()

      # rearrange modsExprns so they Module names are in the same order
      modsExprns <- modsExprns %>%
        mutate(Module = factor(Module, levels = rev(modsSummStats$Module))) # rev as we flip_coords()
      result <- list(expressions = modsExprns, summStats = modsSummStats)

      return(result)
    }
    return(list(expressions = NULL, summStats = NULL))
  }


getModuleValuesForSeries <- function(genesdata,modules,series, ribbon,facet) {
  expressions <- NULL
  if(!is.null(genesdata) && !is.null(modules) && !is.null(series)) {
    expressions <- map_dfr(modules,function(mod){
      modcode <- sub(' .*$', '', mod)
      genesInMod <- tmod::getModuleMembers(modcode)[[modcode]]
      exprs <- genesdata %>%
        filter(Gene %in% genesInMod) %>%
        mutate(Module = mod) %>%
        select(Module, Gene, one_of(series)) %>%
        gather(key = 'Column', value = 'Value',-c(Module,Gene), convert = FALSE,factor_key = FALSE) %>%
        select(Column, Module, Gene, Value)

      return(exprs)
    })

    if(ribbon == "Ribbon") {
      expressions <- expressions %>%
        group_by(Column,Module) %>%
        summarise(
          N = n(),
          SD = sd(Value,na.rm = TRUE),
          Mean = mean(Value,na.rm = TRUE),
          SElo = Mean-SD/sqrt(N),
          SEhi = Mean+SD/sqrt(N)
        ) %>%
        select(Column, Module, Value = Mean, N, SD, SElo, SEhi) %>%
        ungroup()
    }
    
    if(facet == TRUE){
      expressions <- expressions %>%
        separate(Column,into = c('Treatment','Column'),sep = '_', convert = TRUE)
      if(ribbon == "Boxplot")
        expressions <- expressions %>%
        mutate(Column = as.factor(Column)) %>%
        arrange(Treatment,Column,Module)
    } else {
      expressions <- expressions %>%
      mutate(Column = factor(Column, levels = series))
    }
      
  }
  

  return(expressions)
}
