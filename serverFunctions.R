###### GLOBALS for all sessions

load('tmod.rda')


###### Functions
kineticsDF <- function(kinetics, filterExclude = FALSE) {
  df <- unnest(enframe(kinetics, name = "Day")) %>%
    mutate_if(is.character,as.numeric) %>%
    mutate(Y = Max-(Max-Min)/2)
  
  if(filterExclude) df <- filter(df,Exclude == FALSE)
  return(df)
}


kineticsString <- function(kinetics) {
  if(is.null(kinetics)) return(NULL)
  df <- kineticsDF(kinetics,TRUE) %>%
    
    mutate(
      Str = paste0("{",Day," ",paste0(Min,"|",Max),"}")
    )
  return(paste0(df$Str, collapse = ""))
}

dataFrameNotOK <- function(data2check) {
  return(!dataFrameOK(data2check))
}
dataFrameOK <- function(data2check) {
  if(is.null(data2check)) return(FALSE)
  # nrow(data2check)>0 may return logical(0), because nrow(NULL) is NULL, and NULL > 0 is not false but logical(0)!
  # the same response from nrow() applied to lists -> NULL
  # and TRUE && logical(0) = NA! So we return an NA from the test (!is.null(data2check) && nrow(data2check)>0)
  # so use isTRUE to trap this horrible thing called logical(0)
  return(isTRUE(nrow(data2check)>0))
}

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

vaccinesDaysFromColNames <- function(coln) {
  u <- str_split(coln,'_',simplify = TRUE)
  return(list(vaccines = unique(u[,1]), days = unique(u[,2])))
}

columnsFromVaccinesDays <- function(v,d) {
  # grid.expand does not maintain the order but goes V1S1P1V2S2P2 which messes up the levels
 # this is a quick fix. a more elegant apply method no doubt exists avoiding a dataframe
  levs <- map_dfr(v,function(vac){
    vd <- map_chr(d,function(dy) {
      return(paste0(vac,"_",dy))
    })
    return(data.frame(vdy = vd, stringsAsFactors = FALSE))
  })
  return(levs$vdy)
}


getNewData <- function(allData, folderNme) {
  folderpath <- paste0('datafiles/', folderNme) #file.choose()
  annotPath <- paste0(folderpath, '/', 'annotation.rds')
  dataPath <- paste0(folderpath, '/', 'data.rds')
  modPath <- paste0(folderpath, '/', 'modules.rds')
  modmeanPath <- paste0(folderpath, '/', 'modulesMeans.rds')
  
  if (file.exists(dataPath) && file.exists(annotPath)) {
    showNotification("Please wait for data to load…", type = 'message', duration = 3)

        annotation <- read_rds(annotPath)
    allData$annot <- select(annotation,GeneName, SystematicName,Description,Probe = X1, ProbeName)
    
    annotation <- annotation %>%
      select(Probe = X1, ProbeName, Gene = GeneName, Description)

    allData$data<- read_rds(dataPath) %>%
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
  # Note this is ...ForVaccDay, there is NO COLUMN variable
  # protect from not matching colN
  if (dataFrameOK(data) && colN %in% names(data)) {
      if (asGenes == TRUE) {
        data4VaccDay <- data %>%
          # matches will find substrings so force it to match the whole string against colN
          select(Value = matches(paste0('^', colN, '$')), Gene, Description,Probe) %>%
          # Description is missing from asGenes as it is probe-specific
          group_by(Gene) %>%
          summarise(
            N = sum(!is.na(Value)),
            SD = sd(Value, na.rm = TRUE),
            Value = mean(Value, na.rm = TRUE)
            ) %>%
          ungroup() %>%
          # it is much faster to do this on the summary table not within summarise
          mutate(SEM = case_when(N > 1 ~ SD/sqrt(N), TRUE ~ 0)) %>%
          select(Gene,Value,SEM,N)
      } else {
        data4VaccDay <- data %>%
          # matches will find substrings so force it to match the whole string against colN
          # Description is available
          select(Probe, ProbeName, Gene, Value = matches(paste0('^', colN, '$')), Description)
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

getTopGenesInSeries <- function(allData, selData,selCols, facet, genesProbesSelected, splitGenes) {
  if(is.null(allData) || length(selCols) == 0) return(NULL)
  # Note getSortedGenesForVaccDay is ...ForVaccDay, there is NO COLUMN variable in selData, it is just the selected day means
  # so we have to do all the calculations again, for each column now
  
  # asGenes: detect whether it really is as genes based on the selData: if that lacks column Probe then it is
  # asGenes <- ('Probe' %in% names(selData) == FALSE)
  asGenes <-   get("genesOrProbes", envir = .GlobalEnv) == "Gene"

  #at this point we have to decide whether to return gene & probe data non-meaned, or the mean
  # I need to rewrite this as a casewhen or switch
  if (asGenes == TRUE) {
    if (splitGenes == FALSE) {
      seriesData <- allData %>%
        select(Gene, Probe, one_of(selCols)) %>%
        filter(Gene %in% genesProbesSelected)
      
      # lets calc means first
      meansData <- seriesData %>%
        group_by(Gene) %>%
        summarise_at(vars(one_of(selCols)), funs(mean(., na.rm = TRUE))) %>%
        ungroup() %>%
        gather(
          key = 'Column',
          value = 'Value',
          convert = TRUE,
          factor_key = FALSE,
          one_of(selCols)
        )
      
      # now SEM
      semData <- seriesData %>%
        group_by(Gene) %>%
        summarise_at(vars(one_of(selCols)), function(col) {
          sem <-
            ifelse(length(col) > 1, sd(col, na.rm = TRUE) / sqrt(length(col)), 0)
          return(sem)
        }) %>%
        gather(
          key = 'Column',
          value = 'SEM',
          convert = TRUE,
          factor_key = FALSE,
          one_of(selCols)
        )
      
      seriesData <-
        full_join(meansData, semData, by = c("Gene", "Column"))
      
    } else {
      seriesData <- allData %>%
        select(Gene, Probe, one_of(selCols)) %>%
        filter(Gene %in% genesProbesSelected) %>%
        gather(
          key = 'Column',
          value = 'Value',
          convert = TRUE,
          factor_key = FALSE,
          one_of(selCols)
        )
    }
  } else {
    seriesData <- allData %>%
      select(Probe, Gene, one_of(selCols)) %>%
      filter(Probe %in% genesProbesSelected) %>%
      gather(
        key = 'Column',
        value = 'Value',
        convert = TRUE,
        factor_key = FALSE,
        one_of(selCols)
      )
  }
  
  if(facet == TRUE){
    seriesData <- seriesData %>%
      separate(Column,into = c('Treatment','Column'),sep = '_', convert = TRUE) %>%
      mutate(Treatment = factor(Treatment, levels = unique(Treatment)))
  }
  
  return(seriesData)
}

selectedGenesAndModules <- function(selGenes) {
  if(dataFrameOK(selGenes)) {
    selMods <- modules4GeneList(selGenes$Gene,selGenes$Rank)
    selModsOnly <- selMods[selMods$Module != '',]
    return(list(genes = selGenes, modules = selMods, modsOnly = selModsOnly))
  } else {
    return(list(genes = NULL, modules = NULL, modsOnly = NULL))
  }
}

getGenesForRows <- function(genes,start,end){
  if(is.null(genes) || nrow(genes) == 0) return(NULL)

  if(start>end || start>nrow(genes) || end>nrow(genes)){
    showNotification('The rows filter could not be applied. Check From and To match available rows and From < To.', type = 'warning')
    return(genes)
  }

  return(genes[start:end,])
}

getGenesForValues <- function(genes,Min,Max){
  if(is.null(genes) || nrow(genes) == 0 || Min > Max){return(NULL)}
  # selGenes <- genes %>%
  #   filter(between(Value,Min,Max))
  return(filter(genes,between(Value,Min,Max)))
}

getGenesForKinetics <- function(data2Match,kinetics,vacc) {
  kineticsdf <-  kineticsDF(kinetics) %>%
    filter(Exclude == FALSE)

  probes <- map(kineticsdf$Day, function(day){
    Min <- kineticsdf[kineticsdf$Day == day,"Min"][[1]]
    Max <- kineticsdf[kineticsdf$Day == day,"Max"][[1]]
    d <- data2Match %>%
      select(Probe, Value = one_of(paste0(vacc,"_",day))) %>%
      filter(between(Value,Min,Max))
    return(unique(d$Probe))
    })
  
  probesmatching <- reduce(probes, intersect)
  datamatching <- data2Match %>%
    filter(Probe %in% probesmatching)
  return(datamatching)
}

getGenesForSearch <- function(geneslist,search,column,wholeWord){
  if(is.null(geneslist) || is.null(search)) return(NULL)

  # ignore an empty search
  if(search == "") {
    showNotification("Empty searches are ignored", type = 'error')
    return(NULL)
  }
  # strip spaces from genes and probes
  if((column %in% c("Gene","Probe","ProbeName")) && grepl(' ',search)) {
    showNotification("Spaces have been stripped", type = 'warning')
    search <- gsub(" ","",search)
  }
  
  # multiple search ?
  if(grepl(',',search)) {
    searches <- unlist(strsplit(search,','))
    if(wholeWord == TRUE) {searches <- paste0("^",searches,"$")}
    selGenes <- map_dfr(
      searches,
      function(s){
        geneslist[grepl(s,geneslist[[column]], ignore.case = TRUE),]
      }
    )
    if(nrow(selGenes)>0) {
      # avoid duplicate Probes from multiple hits. THIS ASSUMES Probe is unique
      selGenes <- distinct(selGenes, Probe, .keep_all = TRUE)
    }
  } else {
    # no duplicates possible for single search term
    if(wholeWord == TRUE) {search <- paste0("^",search,"$")}
    selGenes <- geneslist[grepl(search,geneslist[[column]], ignore.case = TRUE),]
  }

  return(selGenes)
}

lookupGenesProbes <- function(gene,annot, gorp, wholeWord) {
  if(is.null(gene) || is.null(annot)) return(NULL)

  if(grepl(' ',gene)) {
    showNotification("Spaces have been stripped", type = 'warning')
    gene <- gsub(" ","",gene)
  }

  if(grepl(',',gene)) {
    # multiple search
    gene <- unlist(strsplit(gene,','))
  }
  
  if(wholeWord == TRUE) {
    gene <- paste0('^', gene, '$')
  }
  
  probes <- 
    map_dfr(gene,function(g){
      filter(annot,grepl(g,annot[[gorp]],ignore.case = TRUE))
    }) %>%
    select(GeneName,SystematicName,ProbeName,Probe, Description) %>%
    arrange(GeneName,SystematicName,ProbeName)

  if(nrow(probes) == 0) {
    showNotification("Nothing found", type = 'error')
    return(NULL)
  }
  
  return(probes)
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
                          Title = modDescriptions, 
                          Category = modCats, 
                          stringsAsFactors = FALSE))
      }
      return(data.frame(Rank = generank,Gene = gene,Module = c(''),Title = c(''), Category = '', stringsAsFactors = FALSE))
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
    
    return(modsNameTitle(modsOnly[['Module']],modsOnly[['Title']]))
  } 
  return('')
}

modNameFromMenuTitle <-  function(title) {
  return(sub(' .*$', '', title))
}

getGeneExpressionsInModule <- function(mod, actarmcdDay, allExpressionData, topGenes) {
    # protect from empty data
    if (nchar(mod) > 0 &&
        nchar(actarmcdDay) > 0 &&
        !is.null(allExpressionData) &&
        nrow(allExpressionData) > 0
        )
    {
      # extract just the module name from the name-Title
      selModName <- modNameFromMenuTitle(mod)
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
        
        genesExpression <-  genesExpression %>%
          arrange(Value) %>%
          mutate(Gene = factor(Gene, levels = unique(Gene)))
                 
        if(!is.null(topGenes)) {
          selgenes <- topGenes[['Gene']]
          genesExpression <-  genesExpression %>%
            mutate(Selected = ifelse(Gene %in% selgenes,"►",""))
        } else {
          genesExpression <-  genesExpression %>%
            mutate(Selected = "")
        }
        
        return(genesExpression)
      }
    }
    return(NULL)
  }

getExpressionsForModules <- function(topgenesmods, actarmcdDay, allExpressionData, addPseudoModule, filters) {
  if(
    dataFrameOK(allExpressionData) 
    && !is.null(topgenesmods) # its a list cannot use dataFrameOK
    && dataFrameOK(topgenesmods[['modsOnly']])
    && dataFrameOK(topgenesmods[['genes']])
       ) {
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
                 Title = map_chr(Module, function(m) {
                   tmod$MODULES[tmod$MODULES$ID == mod, ][['Title']]
                 })) %>%
          select(Value, Module, Title)
      })

      if(addPseudoModule == TRUE) {
        # lookup the value of probes in our selected genes for the actamDay which are in topgenesmods[['genes']]
        # just keep Value and give Gene and Title a pseudo name, then rowbind
        selecteGeneExpr <- topgenesmods[['genes']][['Value']]
        modsExprns <- modsExprns %>%
          bind_rows(data.frame(Value = selecteGeneExpr, Module = "Selected", Title = "Selected", stringsAsFactors = FALSE))
      }
      
      modsSummStats <- modsExprns %>%
        group_by(Module, Title) %>%
        summarise(
          Median = median(Value, na.rm = TRUE),
          Mean = mean(Value, na.rm = TRUE),
          SD = sd(Value, na.rm = TRUE),
          N = n()
        ) %>%
        select(Module, Title, everything()) %>%
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
      # strip the treatment in () from module name
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

    if(ribbon == "Lines") {
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
        separate(Column,into = c('Treatment','Column'),sep = '_', convert = TRUE) %>%
        # preserve the order entered into the box, but need to strip out days from series which is treatment_day
        mutate(Treatment = factor(Treatment, levels = unique(sub("_.*","",series))))
      if(ribbon == "Boxplot") {
        expressions <- expressions %>%
        mutate(Column = as.factor(Column))
      }
      expressions <- expressions %>%
        select(Treatment,Column,Module, everything()) %>%
        arrange(Treatment,Column,Module)
    } else {
      expressions <- expressions %>%
      mutate(Column = factor(Column, levels = series)) %>%
      select(Column,Module, everything()) %>%
      arrange(Module,Column)
    }
    
  }
  

  return(expressions)
}

handleClick <- function(data,click,cid,facet,fact,yv) {
  data <- as.data.frame(data)
  if(facet == TRUE) {
    res <- nearPoints(data, click, xvar = "Column", yvar = yv, panelvar1 = 'Treatment') 
  } else {
    if(fact == TRUE) {
    data <- data %>%
      mutate(Column = factor(Column, levels = unique(Column)))
    }
    res <- nearPoints(data, click, xvar = "Column", yvar = yv) 
  }

  if(is.null(res) == FALSE && nrow(res) > 0) {
    showNotification(apply(res[1,], 1, paste, collapse="   ❖   "), duration = 30, id = cid)
  } else {
    removeNotification(id = cid)
  }
}
