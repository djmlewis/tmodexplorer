###### GLOBALS for all sessions

load('tmod.rda')


###### Functions
vaxDayPatterns <- function(colnm) {
  sp <- str_split_fixed(colnm[grepl("_",colnm)],"_", n=Inf)
  xx <- tibble(V1 = sp[,1],V2 = sp[,2]) %>%
    arrange(V1,as.numeric(V2)) %>%
    group_by(V1) %>%
    summarise(V3 = list(V2))
  return(set_names(xx$V3,nm = xx$V1))
}

kineticsDF <- function(kinetics, filterExclude = FALSE) {
  df <- unnest(enframe(kinetics, name = "Day")) %>%
    mutate_if(is.character,as.numeric) %>%
    mutate(Y = Max-(Max-Min)/2)
  dfe <- df
  if(filterExclude) dfe <- filter(df,Exclude == FALSE)
  if(nrow(dfe) == 0) {
    dfe <- df
    showNotification("The kinetics has only skipped windows - they have been all set to open", type = 'error')
  }
  return(dfe)
}

kineticsNotAllExcluded <- function(kinetics) {
  df <- unnest(enframe(kinetics, name = "Day"))
  return(sum(df$Exclude) < nrow(df))
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
    tibble(COLNAMES = grep('_', cnams, value = TRUE)) %>%
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
  # all cols may have unknown values
  allcols <- intersect(allcols,names(alldata))
  if(length(allcols)>0) {
    data <- alldata %>%
      select(one_of(allcols))
    if(ncol(data)>0) {
      return(list(
       Min = floor(min(data, na.rm = TRUE)),
       Max = ceiling(max(data, na.rm = TRUE))))
    } else {
      return(list(Min = 0, Max = 0))
    }
  }
  return(list(Min = 0, Max = 0))
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
  return(list(vaccines = sort(unique(u[,1])), days = sort(as.numeric(unique(u[,2])))))
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
  folderpath <- file.path('datafiles/', folderNme) #file.choose()
  dataPath <- file.path(folderpath, 'data.rds')
  modPath <- file.path(folderpath, 'modules.rds')
  modmeanPath <- file.path(folderpath, 'modulesMeans.rds')
  musclePath <- file.path(folderpath, 'FC_muscle_individuals.rds')
  goDataPath <- file.path(folderpath, 'GO.rds')
  
  if (file.exists(dataPath)) {
    showNotification("Please wait for data to load…", type = 'message', duration = 120, id = "dataLoadingNotification")

    allData$data<- read_rds(dataPath)

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
    
    # try for muscle
    if (file.exists(musclePath)) {
      allData$muscleIndividuals <- read_rds(musclePath)
    }
    
    # try for GO
    if (file.exists(goDataPath)) {
      allData$goData <- read_rds(goDataPath)
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
      annotation <- read_rds(annotPath) 
      
      allData$data <- read_rds(dataPath) %>%
        full_join(annotation, by = 'ProbeName')
      
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

getNSortedGenesForVaccsOnDay <- function(data, vaccs, day, numRows, descend) {
  sortedData <- map_dfc(vaccs, function(v){
    colmn <- paste0(v,"_",day)
    data2Sort <- select(data,Value = one_of(colmn),Gene) %>%
      group_by(Gene) %>%
      summarise(
        Value = mean(Value, na.rm = TRUE)
      ) %>%
      ungroup()
    if(descend) data2Sort <- arrange(data2Sort,desc(Value))
    else data2Sort <- arrange(data2Sort,Value)
    genes <- data.frame(g = data2Sort[1:numRows,][["Gene"]])
    names(genes) <- v
    return(genes)
  }) %>%
    mutate(Rank = 1:numRows)
  return(sortedData)
}

getSortedGenesForVaccDay <- function(data, colN, descend, asGenes,allDays,usingKinetics) {
  # Note this is ...ForVaccDay, there is NO COLUMN variable
  # protect from not matching colN
  
  if (dataFrameOK(data) && colN %in% names(data)) {
    if(descend) stat <- "max" else stat <- "min"
    
      if (asGenes == TRUE) {
        if(allDays == FALSE || usingKinetics == TRUE) {
          # just the selected column
          data4VaccDay <- data %>%
          # matches will find substrings so force it to match the whole string against colN
          select(Value = matches(paste0('^', colN, '$')), Gene) %>%
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
            # find cols containing treatment name
            select(Gene,contains(str_split(colN,"_",simplify = TRUE)[1])) %>%
            group_by(Gene) %>%
            # average all cols by Gene
            summarise_all(mean,na.rm = TRUE) %>%
            ungroup()
          # now have a table of genes in rows and the gene means in cols for each day
          geneNames <- data4VaccDay$Gene # save Gene names
          data4VaccDay <- data4VaccDay %>%
            select(-Gene) %>%
            # row max / min for gene
          mutate(
            ## suppressWarnings is dangerous but used as there are empty sets tested for max and this generates a warning for every row which blocks the programm
            # Value = suppressWarnings(apply(.,1,stat,na.rm = TRUE)),
            Value = apply(.,1,function(v){
              if(sum(is.finite(v)) == 0) return(if_else(descend,-Inf,Inf))
              if(descend) return(max(v,na.rm = TRUE))
              return(min(v,na.rm = TRUE))
            }),
            Gene = geneNames) %>%
            select(Gene,Value)
        }
      } else { 
        # ProbeName values not gene averages
        if(allDays == FALSE || usingKinetics == TRUE) {
          # just the selected column
          data4VaccDay <- data %>%
            # matches will find substrings so force it to match the whole string against colN
            select(ProbeName, Gene, Value = matches(paste0('^', colN, '$')), GeneName, Description)
        } else {
          # all columns with selected treatment
          data4VaccDay <- data %>%
            # find cols containing treatment name
            select(contains(str_split(colN,"_",simplify = TRUE)[1])) %>%
          mutate(
            ## suppressWarnings is dangerous but used as there are empty sets tested for max and this generates a warning for every row which blocks the programm
            # Value = suppressWarnings(apply(.,1,stat,na.rm = TRUE))
            # min and max issue a warning for every row if only NAs are present, so we check for this first and return hi/lo result accordingly
            Value = apply(.,1,function(v){
              if(sum(is.finite(v)) == 0) return(if_else(descend,-Inf,Inf))
              if(descend) return(max(v,na.rm = TRUE))
              return(min(v,na.rm = TRUE))
            })
            )
          data4VaccDay <- cbind(
            select(data4VaccDay,Value),
            select(data, ProbeName, Gene, GeneName, Description)
          )
        }
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
  return(data)
}

getTopGenesInSeries <- function(allData, selData,selCols, facet, genesProbesSelected, splitGenes) {
  if(is.null(allData) || length(selCols) == 0) return(NULL)
  # Note getSortedGenesForVaccDay is ...ForVaccDay, there is NO COLUMN variable in selData, it is just the selected day means
  # so we have to do all the calculations again, for each column now
  
  asGenes <-   get("genesOrProbes", envir = .GlobalEnv) == "Gene"

  # selCols may have unknown combinations
  selCols <- intersect(selCols,names(allData))
  
  
  #at this point we have to decide whether to return gene & probe data non-meaned, or the mean
  # I need to rewrite this as a casewhen or switch
  if (asGenes == TRUE) {
    if (splitGenes == FALSE) {
      seriesData <- allData %>%
        select(Gene, ProbeName, one_of(selCols)) %>%
        filter(Gene %in% genesProbesSelected)
      
      # lets calc means first
      meansData <- seriesData %>%
        group_by(Gene) %>%
        summarise_at(vars(one_of(selCols)), ~ mean(., na.rm = TRUE)) %>%
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
        select(Gene, ProbeName, one_of(selCols)) %>%
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
      select(ProbeName, Gene, one_of(selCols)) %>%
      filter(ProbeName %in% genesProbesSelected) %>%
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
    if(nrow(selMods)>0) selModsOnly <- selMods[selMods$Module != '',]
    else selModsOnly <- selMods

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
  return(filter(genes,between(Value,Min,Max)))
}

getGenesForKinetics <- function(data2Match,kinetics,vacc,asGenes) {
  # kinetics is going to have days not present in the vaccine's days, so we intersect those with the actual column names
  # kinetics <- kinetics[str_split_fixed(intersect(paste0(vacc,"_",names(kinetics)),colnames(data2Match)),"_",2)[,2]]
  kinetics <- kinetics[paste0(vacc,"_",names(kinetics)) %in% colnames(data2Match)]
  # kinetics is now safe to make a DF as it has lost irrelevant days
  kineticsdf <-  kineticsDF(kinetics, TRUE)
  
  col2match <- ifelse(asGenes == TRUE,"Gene","ProbeName")

  if(asGenes == TRUE) {
    selCols <- paste0(vacc,"_",kineticsdf$Day)
    # if asGenes calc means first
    datamatching <- data2Match %>%
      group_by(Gene) %>%
      summarise_at(vars(one_of(selCols)), ~ mean(., na.rm = TRUE)) %>%
      ungroup()
  } else {
    datamatching <- data2Match
  }
  
  matching <- map(kineticsdf$Day, function(day){
    Min <- kineticsdf[kineticsdf$Day == day,"Min"][[1]]
    Max <- kineticsdf[kineticsdf$Day == day,"Max"][[1]]
    d <- datamatching %>%
      select(MatchCol = c(col2match), Value = one_of(paste0(vacc,"_",day))) %>%
      filter(between(Value,Min,Max))
    return(unique(d$MatchCol))
  })
  rowssmatching <- reduce(matching, intersect)
  datamatching <- data2Match %>%
    filter_at(c(col2match),any_vars(. %in% rowssmatching))

  return(datamatching)

}

getSearchItemsFromCommaList <- function(search,wholeWord,stripSpaces, makeUpper = FALSE){
  # !!! returns a string with regex metacharacters when wholeWord == TRUE so NEVER use %in% to match that!!!
  if(is.null(search)) return(NULL)
  if(makeUpper) search <- toupper(search)
  # unlist and stringsplit does not affect a single string, but turns comma separated searches into a vector
  search <- unlist(strsplit(search,',')) 
  # trim spaces from genes and probes
  if(stripSpaces == TRUE) search <- trimws(search, which = "both")#gsub(" ","",search)
  if(wholeWord == TRUE) search <- paste0("^",search,"$")
  return(search)
}

getGenesForSearch <- function(geneslist,search,column,wholeWord, stripSpaces, includeSearch){
  if(is.null(geneslist) || is.null(search)) return(NULL)
  # unlist and stringsplit does not affect a single string, but turns comma separated searches into a vector
  search <- getSearchItemsFromCommaList(search,wholeWord,stripSpaces)
  if(is.null(search)) return(NULL)
  
  rowIndices <- unlist(map(search, ~grep(.,geneslist[[column]], ignore.case = TRUE)))
  if(includeSearch == TRUE) selGenes <- geneslist[rowIndices,]
  else selGenes <- geneslist[-rowIndices,]
  return(selGenes)
}

lookupGenesProbes <- function(search,annot, column, wholeWord, stripSpaces = TRUE) {
  if(is.null(search) || is.null(annot)) return(NULL)

  # unlist and stringsplit does not affect a single string, but turns comma separated searches into a vector
  search <- unlist(strsplit(search,',')) 
  # trim spaces from genes and probes
  if(stripSpaces == TRUE) search <- trimws(search, which = "both")#gsub(" ","",search)
  if(wholeWord == TRUE) search <- paste0("^",search,"$")
  rowIndices <- unlist(map(search, ~grep(.,annot[[column]], ignore.case = TRUE)))
  probes <- annot[rowIndices,]  %>%
    arrange(Gene)
  
  if(nrow(probes) == 0) {
    showNotification("Nothing found", type = 'error')
    return(NULL)
    
  # if(stripSpaces == TRUE) {
  #   showNotification("Spaces have been trimmed", type = 'warning')
  #   gene <- gsub(" ","",gene)
  # }
  # 
  # if(grepl(',',gene)) {
  #   # multiple search
  #   gene <- unlist(strsplit(gene,','))
  # }
  # 
  # if(wholeWord == TRUE) {
  #   if(stripSpaces == TRUE) gene <- paste0("\\b",gene,"\\b")
  #   else gene <- paste0("^",gene,"$")
  # }
  # 
  # 
  # probes <- 
  #   map_dfr(gene,function(g){
  #     filter(annot,grepl(g,annot[[column]],ignore.case = TRUE))
  #   }) %>%
  #   arrange(Gene)

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
      return(NULL)#(data.frame(Rank = generank,Gene = gene,Module = c(''),Title = c(''), Category = '', stringsAsFactors = FALSE))
    })
    return(geneMods)
  }
  return(NULL)
}

moduleDescriptionsForGenes <- function(modsOnly){
  if (length(modsOnly) > 0) {
    modsOnly <- modsOnly %>%
      # remove the row with Selected as module as it has no genes! Its a pseudo module
      filter(Module != 'Selected') %>%
      # keep just Module and Title
      select(Module,Title) %>%
      # we have to remove replicate genes
      distinct()

    return(modsNameTitle(modsOnly[['Module']],modsOnly[['Title']]))
  } 
  return('')
}

modNameFromMenuTitle <-  function(title) {
  return(sub(' .*$', '', title))
}
modNameFromMenuTitlesVector <-  function(title) {
  return(gsub(' .*$', '', title))
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
        select(matches(actarmcdDay), Gene, GeneName, ProbeName)
      
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
                GeneName = exprV[['GeneName']],
                ProbeName = exprV[['ProbeName']],
                stringsAsFactors = FALSE
              )
            return(df)
          }
          return(
            data.frame(
              Module = selModName,
              Gene = gene,
              Value = NA,
              GeneName = NA,
              ProbeName = NA,
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
        group_by(Module) %>%
        summarise(
          Median = median(Value, na.rm = TRUE),
          Mean = mean(Value, na.rm = TRUE),
          SD = sd(Value, na.rm = TRUE),
          N = n()
        ) %>%
        select(Module, everything()) %>%
        arrange(desc(Median)) %>%
        ungroup() %>%
        as.data.frame()

    TitsSummStats <- modsExprns %>%
        group_by( Title) %>%
        summarise(
          Median = median(Value, na.rm = TRUE),
          Mean = mean(Value, na.rm = TRUE),
          SD = sd(Value, na.rm = TRUE),
          N = n()
        ) %>%
        select(Title, everything()) %>%
        arrange(desc(Median)) %>%
        ungroup() %>%
        as.data.frame()
      
      # rearrange modsExprns so they Module names are in the same order
      modsExprns <- modsExprns %>%
        mutate(Module = factor(Module, levels = rev(modsSummStats$Module))) # rev as we flip_coords()
      result <- list(expressions = modsExprns, Module = modsSummStats, Title = TitsSummStats)
      return(result)
  }
    return(list(expressions = NULL, Module = NULL, Title = NULL))
  }


getModuleValuesForSeries <- function(genesdata,modules,series, ribbon,facet) {
  expressions <- NULL
  if(!is.null(genesdata) && !is.null(modules) && !is.null(series)) {

    # series may have unknown combinations
    series <- intersect(series,names(genesdata))
    
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

resForClickBrush <- function(res){
  # CRC306B data has rows with NA so filter out or else hte first row may be NA
  if("Value" %in% colnames(res)) res <- filter(res,!is.na(Value))

  if(is.null(res) == FALSE && nrow(res) > 0 && !is.na(res[1,1])) {
    if("ProbeName" %in% names(res)) spot <- paste(res$ProbeName,collapse = ",")
    else spot <-  NULL
    if("GOterm" %in% names(res)) goterm <- paste(unique(res$GOterm),collapse = ",")
    else goterm <-  NULL
    if("Gene" %in% names(res)) geneMod <- paste(unique(res$Gene),collapse = ",")
    else if("Module" %in% names(res)) {
      # module name and title may be pasted so split out module name
      geneMod <- paste(modNameFromMenuTitlesVector(res$Module),collapse = ",")
    } else geneMod <- NULL
    return(list(Table = xtable(res, auto = TRUE),ProbeName = spot, GeneMod = geneMod, Goterm = goterm))
  }
  return(list(Table = NULL, ProbeName = NULL, GeneMod = NULL, Goterm = NULL))
}

handleClick <- function(data,click,facet,fact,yv,xv = "Column",pv = 'Treatment') {

  if(facet == TRUE) {
    res <- nearPoints(data, click, xvar = xv, yvar = yv, panelvar1 = pv, maxpoints = 1) 
  } else {
    if(fact == TRUE) {
    data <- data %>%
      mutate(Column = factor(Column, levels = unique(Column)))
    }
    res <- nearPoints(data, click, xvar = xv, yvar = yv, maxpoints = 1) 
  }

  return(resForClickBrush(res))
}

handleBrush <- function(data,click,facet,fact,yv,xv = "Column",pv = 'Treatment') {

  if(facet == TRUE) {
    res <- brushedPoints(data, click, xvar = xv, yvar = yv, panelvar1 = pv) 
  } else {
    if(fact == TRUE) {
      data <- data %>%
        mutate(Column = factor(Column, levels = unique(Column)))
    }
    res <- brushedPoints(data, click, xvar = xv, yvar = yv) 
  }
  
  return(resForClickBrush(res))
}

makeTextListOfFilteredGenes <- function(geneprobeDF,alldata,colmnames,listTitle) {
  useProbeName <- "ProbeName" %in% colmnames
  col2Search <- if_else(useProbeName,"ProbeName","Gene")
  geneprobelist <- geneprobeDF[[col2Search]]
  df <- select(alldata,-contains("_")) %>%
    filter_at(col2Search,all_vars(. %in% geneprobelist))


  datalist <- map_chr(colnames(df),function(cn){
    gns <- df %>%
      select_at(vars(cn)) %>%
      filter_all(all_vars(!is.na(.)))
    gns <- unique(gns[[cn]])
    txt <- paste0(cn,"\n",paste(gns, collapse = ','),"\n\n") 
  })
  datalist <- paste(paste(paste(rep_len("—",nchar(listTitle)),collapse = ""),listTitle,paste(rep_len("—",nchar(listTitle)),collapse = ""), sep = "\n"),
                    paste(datalist, collapse = ""),
                    "########## Separated by newline ##########", 
                    gsub(",","\n",paste(datalist, collapse = "")),sep = "\n\n")
  return(datalist)
}

makeTextListOfFilteredModules <- function(moduleDF,listTitle) {
  
  datalist <- map_chr(c("Module","Title","Category"),function(cn){
    gns <- moduleDF %>%
      select_at(vars(cn)) %>%
      filter_all(all_vars(!is.na(.) & nchar(.)>0))
    gns <- unique(gns[[cn]])
    txt <- paste0(cn,"\n",paste(gns, collapse = ','),"\n\n") 
  })
  datalist <- paste(paste(paste(rep_len("—",nchar(listTitle)),collapse = ""),listTitle,paste(rep_len("—",nchar(listTitle)),collapse = ""), sep = "\n"),
                    paste(datalist, collapse = ""),
                    "########## Separated by newline ##########", 
                    gsub(",","\n",paste(datalist, collapse = "")),sep = "\n\n")
  return(datalist)
}
