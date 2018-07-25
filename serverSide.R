
server <- function(input, output, session) {
  
#   #################### Initial Setup #########################
  is_local <- Sys.getenv('SHINY_PORT') == ""
  # removeUI(selector = "#myid", immediate = TRUE)
  # initial hidden setup
  if(is_local == FALSE) {
    hideTab(inputId = "navbarTop", target = "Load transcriptomics")
    hideTab(inputId = "navbarTop", target = "ReadMe")
    hideTab(inputId = "navbarTop", target = "Cells")
    hideTab(inputId = "navbarTop", target = "Cytokines")
  } else {
    hideTab(inputId = "navbarTop", target = "Password")
  }
  # hide the explores until load
  hideTab(inputId = "navbarTop", target = "Explore By Gene")
  hideTab(inputId = "navbarTop", target = "Explore By Module")
  # hideTab(inputId = "navbarTop", target = "Cells")
  hideTab(inputId = "navbarTop", target = "Lookup")
  
  assign("sortCol_Probes",NULL, envir = .GlobalEnv)
  assign("expressionValueRangeVaccDay",list(Max = 0, Min = 0), envir = .GlobalEnv)
  expressionValueRangeVaccAllDays <- reactiveVal(list(Max = 0, Min = 0))
  assign("dataValueRange",expressionValueRangeVaccDay, envir = .GlobalEnv)
  shapeKinetics <- reactiveVal(NULL)
  
  assign("dayPatterns",read_rds("dayPats.rds"), envir = .GlobalEnv)
  assign("vaccineColours",read_rds("vaccinecolours.rds"), envir = .GlobalEnv)
  
  
#   #################### Password #########################
  password <- read_rds("p")
  observeEvent(input$buttonPassword, {
    if (input$password == password) {
      hideTab(inputId = "navbarTop", target = "Password")
      showTab(inputId = "navbarTop", target = "Load transcriptomics", select = TRUE)
      showTab(inputId = "navbarTop", target = "ReadMe")
      showTab(inputId = "navbarTop", target = "Cells")
      showTab(inputId = "navbarTop", target = "Cytokines")
    } else {
      sendSweetAlert(session, type = 'error', title = "Password", text = "That password is not recognised")
    }
    })
  
  
#   #################### Loading data #########################
#   
  # list local data files on the server
  files <- sort(basename(list.dirs(path = 'datafiles', recursive = FALSE)))
  updatePickerInput(session, 'selectDataFI', choices = files) # list(`Fold Increase From Baseline` = files[grepl("Fold", files)], `Raw Expression Values` = files[!grepl("Fold", files)]))

  allData <- reactiveValues(data = NULL,colNames = NULL, folder = NULL,folderpath = NULL, modules = NULL, modulesMeans = NULL, annot = NULL)
  observeEvent(input$buttonLoadDataFI, {if (getNewData(allData,input$selectDataFI) == TRUE) {updateLoadControls()}},ignoreInit = TRUE)
  observeEvent(input$fileInputUploadData,{if(loadUploadedData(allData,input$fileInputUploadData,input$textInputUploadFileName)) {updateLoadControls()}})
  
  output$buttonsavedatatableAll <- downloadHandler(filename = function(){paste0(allData$folder,".csv")},
    content = function(file) {write.csv(allData$data, file, row.names = FALSE)})
  
  output$datatableAll <- renderDataTable({allData$data},options = list(searching = TRUE))
  
  updateExpressionValueRangeVaccDay <- function(selCol){
    if(!is.null(selCol) && all(selCol %in% allData$colNames)){
      exprangeVD <- getMaxMinValueFromData(allData$data,c(selCol))
      assign("expressionValueRangeVaccDay",exprangeVD, envir = .GlobalEnv)
      
      updateNumericInput(session,'numberExpressionMin',value = exprangeVD[['Min']])
      updateNumericInput(session,'numberExpressionMax',value = exprangeVD[['Max']])
      
      # removeNotification(id = "updateExpressionValueRangeVaccDay")
      # showNotification(id = "updateExpressionValueRangeVaccDay", paste0("Expression Max Min Reset To ", ifelse(length(selCol)==1,"Selected Column","Whole Dataset"), " Max Min"))
    }
  }
  
  updateModuleMinMax <- function(selCol){
    if(!is.null(selCol) && all(selCol %in% allData$colNames) && dataFrameOK(allData$modulesMeans)){
      modrange <- getMaxMinValueFromModulesData(allData,c(selCol), input$mcheckboxModuleMedians)
      updateNumericInput(session,'mnumberExpressionMin',value = modrange[['Min']])
      updateNumericInput(session,'mnumberExpressionMax',value = modrange[['Max']])
      # removeNotification(id = "updateModuleMinMax")
      # showNotification(id = "updateModuleMinMax", paste0("Module Max Min Reset To ", ifelse(length(selCol)==1,"Selected Column","Whole Dataset"), " Max Min"))
    }
  }
  
  # KINETICS MATCHING ###############
  assign("copiedDayKinetics",NULL, envir = .GlobalEnv)
  
  
  vaccDaysInDataset <- function() {
    vaccinesDaysFromColNames(allData$colNames)$days
  }
  resetShapeNumericsToDataset <- function() {  
    updateNumericInput(session,"numberShapeDayMin",value = dataValueRange[["Min"]])
    updateNumericInput(session,"numberShapeDayMax",value = dataValueRange[["Max"]])
  }
  
  resetShapeNumericsToVaccine <- function(){
    if(is.null(dayPatterns[[input$selectColumnVaccine]])) {showNotification(type = 'error', ui = paste0("There is a problem with data formatting - no day numbers could be found for ",input$selectColumnVaccine))}
    colsToLookup <- paste0(input$selectColumnVaccine,"_",dayPatterns[[input$selectColumnVaccine]])
    rangeVAD <- getMaxMinValueFromData(allData$data,colsToLookup)
    expressionValueRangeVaccAllDays(rangeVAD)
    updateNumericInput(session,"numberShapeDayMin",value = rangeVAD[["Min"]])
    updateNumericInput(session,"numberShapeDayMax",value = rangeVAD[["Max"]])
  }
  
  defaultKineticsForDataset <- function(data) {
    days <- vaccDaysInDataset()
    set_names(replicate(length(days),
                        {data_frame(Min = data[["Min"]],
                                    Max = data[["Max"]], 
                                    Exclude = FALSE)}, 
                        simplify = FALSE),
              days)
  }
  
  setupKineticsFromDataset <- function(days,data) {
    updatePickerInput(session, 'selectShapeDay', choices = days)
    if(data == "data") {
      shapeKinetics(defaultKineticsForDataset(dataValueRange))
      resetShapeNumericsToDataset()
    } else {
      resetShapeNumericsToVaccine()
      shapeKinetics(defaultKineticsForDataset(expressionValueRangeVaccAllDays()))
    }
  }
  
  updateDayKineticsToDF <- function(df) {
    updateAwesomeCheckbox(session,"checkboxShapeSkipDay",value = df$Exclude[[1]])
    updateNumericInput(session,"numberShapeDayMin",value = df$Min[[1]])
    updateNumericInput(session,"numberShapeDayMax",value = df$Max[[1]])
  }
  
  observeEvent(
    input$selectShapeDay,
    {
      updateDayKineticsToDF(shapeKinetics()[[input$selectShapeDay]])
    }
  )
  
  observeEvent(
    input$buttonCopyValuesShapeData,
    {
      assign("copiedDayKinetics",data_frame(Exclude = input$checkboxShapeSkipDay ,Min = input$numberShapeDayMin, Max = input$numberShapeDayMax), envir = .GlobalEnv)
    }
  )
  
  observeEvent(
    input$buttonPasteValuesShapeData,
    {
      if(!is.null(copiedDayKinetics)) {
        saveKineticsDayValues(copiedDayKinetics$Exclude[1],input$selectShapeDay,copiedDayKinetics$Min[1],copiedDayKinetics$Max[1])
        updateDayKineticsToDF(copiedDayKinetics)
      }
    }
  )
  
  observeEvent(
    input$buttonResetValuesShapeData,
    {
      resetShapeNumericsToDataset()
    }
  )
  
  
  observeEvent(
    input$buttonResetValuesShapeVaccine,
    {
      resetShapeNumericsToVaccine()
    }
  )

  observeEvent(
    input$buttonResetKineticsData,
    {
      setupKineticsFromDataset(vaccDaysInDataset(),"data")
    }
  )
  observeEvent(
    input$buttonResetKineticsTreat,
    {
      setupKineticsFromDataset(vaccDaysInDataset(),"treat")
    }
  )
  
  
  saveKineticsDayValues <- function(ignore,day,minVal,maxVal) {
    curKinetics <- shapeKinetics()
    if(ignore == TRUE) {
      curKinetics[[day]] <- data_frame(Min = dataValueRange[["Min"]],Max = dataValueRange[["Max"]], Exclude = TRUE)
      if(kineticsNotAllExcluded(curKinetics)) {
        resetShapeNumericsToDataset()
        shapeKinetics(curKinetics)
      } else {
        showNotification("This window cannot be excluded as at least one window must be defined", type = 'error')
      }
    } else {
      curKinetics[[day]] <- data_frame(Min = minVal,Max = maxVal, Exclude = ignore)
      shapeKinetics(curKinetics)
    }
  }
  
  observeEvent(
    input$click_plotShapeMiniplot, 
    {
      df <-  kineticsDF(shapeKinetics()) %>%
        mutate(DayF = 1:length(Day))
      res <- nearPoints(df, input$click_plotShapeMiniplot, xvar = "DayF", yvar = "Y", maxpoints = 1,threshold = 10) 
      if(nrow(res)>0) {
        updatePickerInput(session,"selectShapeDay", selected = as.character(res$Day[1]))
      }
    })
  
  observeEvent(
    input$dblclick_plotShapeMiniplot, 
    {
      df <-  kineticsDF(shapeKinetics()) %>%
        mutate(DayF = 1:length(Day))
      res <- nearPoints(df, input$dblclick_plotShapeMiniplot, xvar = "DayF", yvar = "Y", maxpoints = 1,threshold = 10) 
      if(nrow(res)>0) {
        # dayF is an index 1...ndays. Use that to lookup the index of day that has the Day value
        selday <- as.character(res$Day[1])
        selKinDF <- shapeKinetics()[[selday]]
        saveKineticsDayValues(!selKinDF$Exclude[1],selday,dataValueRange[["Min"]],dataValueRange[["Max"]])
        updatePickerInput(session,"selectShapeDay", selected = selday)
        updateDayKineticsToDF(shapeKinetics()[[selday]])
      }
    })
  
  observeEvent(
    input$buttonShapeSaveDay,
    {
      saveKineticsDayValues(input$checkboxShapeSkipDay,input$selectShapeDay,input$numberShapeDayMin,input$numberShapeDayMax)
    }
  )
  
  output$buttonSaveShapeKinetics <- downloadHandler(filename = function(){
    return(paste0(allData$folder,kineticsString(shapeKinetics()), ".rds"))},
    content = function(file) {write_rds(list(FileName = allData$folder, KinsType = "actual", Kinetics = shapeKinetics()), file)})
  
  observeEvent(input$buttonLoadShapeKinetics,
   {
     # check its a valid kinetics file
     if (!is.null(input$buttonLoadShapeKinetics)) {
       kinsfile <- read_rds(input$buttonLoadShapeKinetics$datapath)
       if (!is.null(kinsfile) && !is.null(kinsfile[["KinsType"]])) {
         # get a default kinetics first as we may have different days and can keep the defaults in
         newkins <- kinsfile[["Kinetics"]]
         defkins <- defaultKineticsForDataset(dataValueRange)

         defnames <- names(defkins)
         # amend the kinetics with the new one where days match, dont add irrelevant ones

         walk(names(newkins), function(name) {
           if (name %in% defnames) {
             defkins[[name]] <<- newkins[[name]]
           }
         })
         shapeKinetics(defkins)

         #input$buttonLoadShapeKinetics$name is the filename
         if(identical(defnames,names(newkins))) showNotification(paste0("Imported shape kinetics from ",input$buttonLoadShapeKinetics$name),type = "message")
         else showNotification(paste0("Imported shape kinetics from ",input$buttonLoadShapeKinetics$name," - however they seem to be from a different dataset and so may be incorrect"),type = "warning")
       } else {showNotification(paste0(input$buttonLoadShapeKinetics$name, " appears not to contain valid kinetics"),type = "error")}
     } else {showNotification(paste0("There was noting to import from ",input$buttonLoadShapeKinetics$name),type = "error")}
   })
  
  
  ggPlotShapeMiniplot <- reactive({getGGplotShapeMiniplot(shapeKinetics(),expressionValueRangeVaccAllDays())})
  output$plotShapeMiniplot <- renderPlot({ggPlotShapeMiniplot()})
  
  
  # LOADING ##########
  
  updateLoadControls <- function(){
    
    # show hide the nav tabs to reflect we have loaded data, rehide any needing rehiding post select
    showTab(inputId = "navbarTop", target = "Explore By Gene")
    showTab(inputId = "navbarTop", target = "Explore By Module")
    # showTab(inputId = "navbarTop", target = "Cells")
    showTab(inputId = "navbarTop", target = "Lookup")
    
    # we may have been on a different pane so re-select Select
    updateNavbarPage(session,'navProbe',selected = 'Select Genes')
    hideTab(inputId = "navProbe", target = "Selected Genes")
    hideTab(inputId = "navProbe", target = "Genes:Series")
    hideTab(inputId = "navProbe", target = "Genes->Modules")
    hideTab(inputId = "navProbe", target = "Modules")
    hideTab(inputId = "navProbe", target = "Module->Genes")
    hideTab(inputId = "navProbe", target = "Modules:Series")
    
    
    show(id = "textDataNameHeader")
    show(id = "textDataNameModsHeader")
    show(id = "textDataNameProbesHeader")
    
    # we may have been on a different pane so re-select Select
    updateNavbarPage(session,'navModule',selected = 'Select Modules')
    hideTab(inputId = "navModule", target = "Selected Modules")
    hideTab(inputId = "navModule", target = "Module->Genes")
    hideTab(inputId = "navModule", target = "Modules:Series")

    # reset to NULL previous selections
    assign("sortCol_Probes",NULL, envir = .GlobalEnv)
    topGenesAndModules(NULL)
    topModulesSelected(NULL)
    
    
    # these must be updated here as they do not observe allData
    vaccDays <- vaccinesDaysFromColNames(allData$colNames)
    updatePickerInput(session, 'selectColumnVaccine', choices = vaccDays$vaccines)
    updatePickerInput(session, 'selectVaccNet', choices = vaccDays$vaccines)
    
    updatePickerInput(session, 'selectVacDaysToNet', choices = character(0), selected = character(0))
    
    
    updateSelectInput(session, 'selectVaccinesForSeries', choices = vaccDays$vaccines, selected = character(0))
    updateSelectInput(session, 'selectDaysForSeries', choices = vaccDays$days, selected = character(0))
    updatePickerInput(session, 'selectGenesProbesForSeries', choices = character(0), selected = character(0))
    
    updateSelectInput(session, 'selectColumnForModuleSeriesVaccines', choices = vaccDays$vaccines, selected = character(0))
    updateSelectInput(session, 'selectColumnForModuleSeriesDays', choices = vaccDays$days, selected = character(0))
    
    modulesAndFiltersText("")
    filtersText("")
    
    output$plotTopGenesSeries <- renderPlot({NULL})
    output$datatableTopGenesSeries <- renderDataTable({NULL})
    output$plotModuleSeries <- renderPlot({NULL})
    output$datatableModuleSeries <- renderDataTable({NULL})
    output$mdatatableModuleLookup <- renderDataTable({NULL})
    output$datatableGeneLookup <- renderDataTable({NULL})
    
    

    # modules DO NOT RESPOND. NEED TO FIX
    # updatePickerInput(session, inputId = 'mselectColumn', choices = allData$colNames)
    updatePickerInput(session, 'mselectColumnVaccine', choices = vaccDays$vaccines)
    updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', choices = vaccDays$vaccines, selected = character(0))
    updateSelectInput(session, 'mselectColumnForModuleSeriesDays', choices = vaccDays$days, selected = character(0))
    
    updateSelectInput(session, 'mselectPlotModulesInSeries', choices = character(0))
    updatePickerInput(session, 'mselectModuleForGenes', choices = NULL)
    updateSelectInput(session, 'mselectModuleTitles', choices = sort(unique(allData$modulesMeans[['Title']])))
    updateSelectInput(session, 'mselectModuleAllModules', choices = sort(unique(modsNameTitle(allData$modulesMeans[['Module']],allData$modulesMeans[['Title']]))))
    
    # based on menu we just update calc max min as the event is not triggered.
    updateExpressionValueRangeVaccDay(allData$colNames)
    updateModuleMinMax(allData$colNames)
    # setup the data min max one time
    assign("dataValueRange",expressionValueRangeVaccDay, envir = .GlobalEnv)
    # setup kinetics
    setupKineticsFromDataset(vaccDays$days,"data")
  }

  # these do resopnd OK outside this scope but put here for neatness
dataAndFiltersText <- reactiveVal(value = "")
filtersText <- reactiveVal(value = "")
modulesAndFiltersText <- reactiveVal(value = "")
output$textDataName <- renderText({paste0("\U1F4C2 ",allData$folder)})
output$textDataNameProbes <- renderText({paste0("\U1F4C2 ",allData$folder)})
output$textDataNameMods <- renderText({paste0("\U1F4C2 ",allData$folder)})
output$textFiltersProbes <- renderText({paste0("\U1F50D ",filtersText())})
output$textFiltersMods <- renderText({paste0("\U1F50D ",modulesAndFiltersText())})


#################### PROBES #####################

  #################### Selecting Columns #########################
  # select the genes and identify associated modules

makeSelectColumnDayStyleGrey <- function(makeGrey){
  if(makeGrey == TRUE) {
    runjs("$('#selectColumnDay').selectpicker('setStyle', 'btn-success', 'remove');")
    runjs("$('#selectColumnDay').selectpicker('setStyle', 'btn-default', 'add');")
  } else {
    runjs("$('#selectColumnDay').selectpicker('setStyle', 'btn-default', 'remove');")
    runjs("$('#selectColumnDay').selectpicker('setStyle', 'btn-success', 'add');")
  }
  
}
### selecting events - probes
respondToChangeColumn <- function(picker) {
  assign("sortCol_Probes",columnsFromVaccinesDays(input$selectColumnVaccine,input$selectColumnDay), envir = .GlobalEnv)
  updateExpressionValueRangeVaccDay(sortCol_Probes)
  if(picker == 'vacc') {
    resetShapeNumericsToVaccine()
  }
}

session$onFlushed(function() {
  # the selectColumnDay will be reset to the default color, so we have to reset the style in case we have kinetics or All days
  # this is inefficient as it is called every time we refresh anything but has to be like this to overcome situation where Vaccine select changes and Day select does not - it is this refresh we must trap
  makeSelectColumnDayStyleGrey(isolate(input$checkboxRowsAnyDay == TRUE || (input$checkboxSelectValues == TRUE && input$radioFilterByRowKinetics == 'kinetics')))
}, once = FALSE)

observeEvent(
  {
    input$selectColumnDay
  },
  {
    respondToChangeColumn("day")
  })
observeEvent(
  {
    input$selectColumnVaccine
  },
  {
    if(is.null(dayPatterns[[input$selectColumnVaccine]])) {showNotification(type = 'error', ui = paste0("There is a problem with data formatting - no day numbers could be found for ",input$selectColumnVaccine))}
    updatePickerInput(session, 'selectColumnDay', choices = dayPatterns[[input$selectColumnVaccine]])
    respondToChangeColumn("vacc")
  })

# observeEvent(
#   {
#     input$radioFilterByRowKinetics
#   },
#   {
#     if(input$radioFilterByRowKinetics == "row") {
#       show("checkboxRowsAnyDay")
#     } else {
#       hide("checkboxRowsAnyDay")
#     }
#   }, ignoreInit = TRUE)
# 
# 
# observeEvent(
#   input$checkboxSelectValues,
#   {
#     if(input$checkboxSelectValues == FALSE) show("checkboxRowsAnyDay")
#     else {
#       if(input$radioFilterByRowKinetics == 'kinetics') hide("checkboxRowsAnyDay")
#       else show("checkboxRowsAnyDay")
#     }
#   })


observeEvent(
    input$buttonResetValuesRangeCol,
    {updateExpressionValueRangeVaccDay(sortCol_Probes)})

observeEvent(
    input$buttonResetValuesRangeData,
    {updateExpressionValueRangeVaccDay(allData$colNames)})

  assign("warnedAboutProbeRows",FALSE, envir = .GlobalEnv)
  observeEvent(
    input$checkboxSelectRows,
    {if(warnedAboutProbeRows == FALSE && (input$checkboxSelectRows == FALSE || input$numberGenesStart - input$numberGenesEnd > 100)) {
      assign("warnedAboutProbeRows",TRUE, envir = .GlobalEnv)
    }})
  
  observeEvent(
    input$checkboxGeneSearchWholeWord,
    {
      if(input$checkboxGeneSearchWholeWord == FALSE && input$selectKeywordColumn != 'Description') {
        showNotification("When searching columns other than 'Description', it is better to keep Whole Word selected otherwise you will match partial names - which may be not what you intend.", type = 'warning')
      }
    }, ignoreInit = TRUE)
  
  
############### Apply Selection ### ####
  topGenesAndModules <- reactiveVal()
  topGenesAndModules(NULL)
  observeEvent(
    input$buttonApplySelection,
    {
        if(!is.null(sortCol_Probes)) {
          if(input$checkboxSelectKeyword == FALSE && input$checkboxSelectValues == FALSE && input$checkboxSelectRows == FALSE && input$radioFilterByRowKinetics == 'row') {
            sendSweetAlert(session, type = 'error', title = "Too Many Rows", text = "You must have at least one filter selected or it will try to return and plot over 40,000 rows")
            } else {
              topGenesAndModules(NULL)
              
              showNotification("Please wait for filters to be applied…", type = 'message', duration = 3, id = "buttonApplySelection")

                            # show the tabs as we have selected probes
              showTab(inputId = "navProbe", target = "Selected Genes")
              showTab(inputId = "navProbe", target = "Genes:Series")
              showTab(inputId = "navProbe", target = "Genes->Modules")
              showTab(inputId = "navProbe", target = "Modules")
              showTab(inputId = "navProbe", target = "Module->Genes")
              showTab(inputId = "navProbe", target = "Modules:Series")
              
              filterSubText <-  ""
              # keyword acts on allData$data, ignores genes/probes so start with that to reduce
              geneslist <- allData$data
              
              # apply the filters sequentially, do regex first before gene averages in getSortedGenesForVaccDay strips description
              if(input$checkboxSelectKeyword == TRUE) {
                # ignore an empty search
                if(input$textInputKeyword == "") {
                  showNotification("Empty keyword searches are ignored. No keyword filter has been applied.", type = 'error')
                } else {
                  if(input$checkboxGeneSearchWholeWord == FALSE && input$selectKeywordColumn != 'Description') {
                    showNotification("Warning: when searching columns other than 'Description' without having Whole Word selected you will match partial names - which may be not what you intend.", type = 'warning')
                  }
                  geneslist <- getGenesForSearch(geneslist,input$textInputKeyword,input$selectKeywordColumn,input$checkboxGeneSearchWholeWord)
                  if(dataFrameOK(geneslist)) {filterSubText <-  paste0(filterSubText,'"',input$textInputKeyword,'" ')}
                }
              } 

              # kinetics also acts on allData$data, working on spots so continue with that to reduce
              assign("selectKinetics",input$checkboxSelectValues == TRUE && input$radioFilterByRowKinetics == 'kinetics', envir = .GlobalEnv)
              if(selectKinetics == TRUE && dataFrameOK(geneslist)) {
                geneslist <- getGenesForKinetics(geneslist,shapeKinetics(), input$selectColumnVaccine,input$checkboxProbesGenes)
                if(dataFrameOK(geneslist)) {
                  filterSubText <-  paste0(filterSubText," match ",kineticsString(shapeKinetics()))
                } 
              }
              
              # all the other filters used sorted and possibly gene averaged data so get that now
              geneslist <- getSortedGenesForVaccDay(geneslist,sortCol_Probes,input$checkboxDescending,input$checkboxProbesGenes,input$checkboxRowsAnyDay,input$radioFilterByRowKinetics == 'kinetics')
              
              # single column value filter
              if(input$checkboxSelectValues == TRUE && input$radioFilterByRowKinetics != 'kinetics' && dataFrameOK(geneslist)){
                geneslist <- getGenesForValues(geneslist,input$numberExpressionMin,input$numberExpressionMax)
                filterSubText <-  paste0(filterSubText,' Values ',input$numberExpressionMin,'>=<',input$numberExpressionMax,' ')
              }
              
              # rows filter 
              if(input$checkboxSelectRows == TRUE && dataFrameOK(geneslist)){
                geneslist <- getGenesForRows(geneslist,input$numberGenesStart,input$numberGenesEnd)
                filterSubText <-  paste0(filterSubText,' Rows ',input$numberGenesStart,':',input$numberGenesEnd,' ')
              }
                
              # finalise the filters text
              if(dataFrameOK(geneslist)) {
                if(nchar(filterSubText) > 0) {
                  filtersText(
                    paste0(gsub('_',' ',sortCol_Probes),' ❖ ',filterSubText,' ❖ ',
                           ifelse(input$checkboxDescending == TRUE, ' Sort ↓ ',' Sort ↑ '), ifelse(input$checkboxRowsAnyDay == TRUE," All Days ❖ ",' ❖ '),
                           ifelse(input$checkboxProbesGenes == TRUE, ' Gene Means',' Spots')
                    ))
                  dataAndFiltersText(paste0(allData$folder,': ',filtersText()))
                } else {
                  filtersText(paste0(gsub('_',' ',sortCol_Probes),' ❖ [No filters] ❖ ',
                                     ifelse(input$checkboxDescending == TRUE, ' Sort Descending ❖ ',' Sort Ascending ❖ '),
                                     ifelse(input$checkboxProbesGenes == TRUE, ' Gene Means',' Spots')))
                  dataAndFiltersText(paste0(allData$folder,': ',filtersText()))
                }
              } else {
                filtersText("")
                dataAndFiltersText("")
              }

              if(dataFrameOK(geneslist)) {
                show(id = "navProbeHeader")
              } else {
                hide(id = "navProbeHeader")
              }
              
              if(dataFrameOK(geneslist)) {
                nrowsGeneList <- nrow(geneslist)
                if(nrowsGeneList>input$rowsLimitNumeric) {
                  sendSweetAlert(
                    session = session,
                    type = "warning",
                    title = "Too many rows returned by filter",
                    text = paste0("A total of ",nrowsGeneList," rows have been returned which exceeds the limit of ",input$rowsLimitNumeric ," set on the Rows Limit Input. The number of rows will be capped at ",input$rowsLimitNumeric,". To increase this change the setting and repeat Apply Filters. WARNING -  more than 100 rows may freeze the app for a very long time."),
                    btn_labels = c("OK")
                  )
                  geneslist <- geneslist[1:input$rowsLimitNumeric,]
                }
              }
                
              ############ lookup the genes and modules
              if(dataFrameOK(geneslist)) {
                topGenesAndModules(selectedGenesAndModules(geneslist))
                  removeNotification(id = "buttonApplySelection")
                  nModules <- ifelse(nrow(topGenesAndModules()[['modules']]) == 0,"0",length(unique(topGenesAndModules()[['modules']][["Module"]])))
                  showNotification(paste0("Found: ",nrow(topGenesAndModules()[['genes']]),
                    ifelse(input$checkboxProbesGenes == TRUE, ' genes', ' probes'), " and ",
                    nModules, " modules",
                    " using filter: ", filtersText()), type = 'message')
              } else {
                topGenesAndModules(list(genes = NULL, modules = NULL, modsOnly = NULL))
                removeNotification(id = "buttonApplySelection")
                showNotification(paste0("Found 0 Spots and 0 Modules"," using filter: ", filtersText()), type = 'warning')
              }
            }
        } else {
        showNotification("A column to sort must always be selected, even if just filtering by regex", type = 'error')
      }
    }
  )
  
  assign("genesOrProbes","Gene", envir = .GlobalEnv)

  observeEvent(
    topGenesAndModules(),
    {
      # these are non-reactive and need a manual reboot
      output$plotModuleSeries <- renderPlot({NULL})
      output$datatableModuleSeries <- renderDataTable({NULL})
      updateplotModuleSeriesBRUSH(list(Table = NULL, Spot = NULL, Gene = NULL))
      
      updateSelectInput(session, 'selectColumnForModuleSeriesVaccines')
      updateSelectInput(session, 'selectColumnForModuleSeriesDays')
      
      output$plotTopGenesSeries <- renderPlot({NULL})
      output$datatableTopGenesSeries <- renderDataTable({NULL})
      updateplotTopGenesSeriesBRUSH(list(Table = NULL, Spot = NULL, Gene = NULL))
      
      updateSelectInput(session, 'selectVaccinesForSeries')
      updateSelectInput(session, 'selectDaysForSeries')
      # need to determine if Spots or genes
      pgColname <- ifelse('Spot' %in% names(topGenesAndModules()[['genes']]) == FALSE,"Gene","Spot")
      assign("genesOrProbes",pgColname, envir = .GlobalEnv)
      updateSelectInput(session, 'selectGenesProbesForSeries', label = pgColname, choices = topGenesAndModules()[['genes']][[pgColname]], selected = topGenesAndModules()[['genes']][[pgColname]])
      
      if(genesOrProbes == "Gene") {show(id = "checkboxShowProbesOfGenesSeries")} 
      else {hide(id = "checkboxShowProbesOfGenesSeries")}
    }
  )
  
  
  #################### Top Spots #########################
  # output top genes
  output$datatableTopGenesUp <- renderDataTable({topGenesAndModules()[['genes']]})
  output$buttonSaveTableTopGenesUpPlot <- downloadHandler(filename = function(){paste0("Selected Spots-Genes.png")},
    content = function(file) {plotDataTable(topGenesAndModules()[['genes']],file,35)})
  
  dataFilterStr <- function(t) {
    switch (t,
      'g' = return(paste0(allData$folder,'\n# ',filtersText())),
      'm' = return(paste0(allData$folder,'\n# ',modulesAndFiltersText()))
    )
  }
  
  output$buttonSaveTableProbes <- downloadHandler(filename = function(){paste0("Selected Spots-Genes.csv")},
    content = function(file) {write.csv(topGenesAndModules()[['genes']], file, row.names = FALSE)})
  
  output$buttonSaveListGenes <- downloadHandler(filename = function(){paste0("Selected ",input$pickerSaveListTopGenes,".txt")},
   content = function(file) {write_lines(paste0(paste(unique(topGenesAndModules()[['genes']][[input$pickerSaveListTopGenes]]), collapse = ','),'\n\n# ',dataFilterStr('g')), file)})
  
  #################### Top Spots Series #########################
  # topGenesInSeries <- NULL
  assign("topGenesInSeries",NULL, envir = .GlobalEnv)
  
  observeEvent(
    {
      input$buttonPlotSeries
    },
    {
      columnsForSeries <- columnsFromVaccinesDays(input$selectVaccinesForSeries,input$selectDaysForSeries)
      assign("topGenesInSeries",
             getTopGenesInSeries(allData$data,topGenesAndModules()[['genes']],columnsForSeries,input$checkboxSplitSeries,
                                 input$selectGenesProbesForSeries,input$checkboxShowProbesOfGenesSeries), 
             envir = .GlobalEnv)
      
      output$datatableTopGenesSeries <- renderDataTable({topGenesInSeries})
      updateplotTopGenesSeriesBRUSH(list(Table = NULL, Spot = NULL, Gene = NULL))
      
      ggplotTopGenesInSeries <- plotTopGenesInSeries(topGenesInSeries,
        input$checkboxShowPointsSeries,input$checkboxShowSEMSeries,input$checkboxShowLegendSeries,dataAndFiltersText(),input$checkboxSplitSeries,
        input$checkboxShowZeroSeries,input$radioBoxLineProbesSeries,sortCol_Probes, input$checkboxShowGridSeries,input$checkboxShowProbesOfGenesSeries,
        input$numericNumPanelsTopGenesSeries, shapeKinetics(),
        input$checkboxShowKineticsSeries & selectKinetics) # AND the checkbox and we are using kinetics

      output$plotTopGenesSeries <- renderPlot({ggplotTopGenesInSeries} ,res = 72)
      output$plotTopGenesSeriesSIZE <- renderUI({tagList(
                                                         plotOutput("plotTopGenesSeries", 
                                                                    height = isolate(input$numberPlotTopGenesSeriesSIZEheight),
                                                                    click = "click_plotTopGenesSeries",
                                                                    brush = "brush_plotTopGenesSeries"
                                                         ))})
      output$buttonPNGplotTopGenesSeries <- downloadHandler(filename = function(){paste0("Selected Genes As Series.png")},
        content = function(file) {printPlotPNG(ggplotTopGenesInSeries,file,session$clientData[["output_plotTopGenesSeries_height"]],session$clientData[["output_plotTopGenesSeries_width"]])})
      
    })
  
  updateplotTopGenesSeriesBRUSH <- function(res) {
    output$plotTopGenesSeriesBRUSH <- renderTable({res$Table}, striped = TRUE)
    output$plotTopGenesSeriesGENEMOD <- renderText({res$GeneMod})
    output$plotTopGenesSeriesSPOT <- renderText({res$Spot})
  }
  
  observeEvent(
    input$click_plotTopGenesSeries, 
    {if(input$radioBoxLineProbesSeries != 'Boxplot') 
      {
        res <- handleClick(topGenesInSeries,input$click_plotTopGenesSeries, input$checkboxSplitSeries,TRUE,"Value")
        if(!is.null(res$Table)) {
          updateplotTopGenesSeriesBRUSH(res)
        }
      }
    })
  
  observeEvent(input$brush_plotTopGenesSeries, 
   {if(input$radioBoxLineProbesSeries != 'Boxplot') {
     updateplotTopGenesSeriesBRUSH(handleBrush(topGenesInSeries,input$brush_plotTopGenesSeries,input$checkboxSplitSeries,TRUE,"Value"))
   }
   })
  
  observeEvent(input$buttonAddAllVaccinesSeries,{updateSelectInput(session, 'selectVaccinesForSeries', selected = vaccinesDaysFromColNames(allData$colNames)[['vaccines']])})
  observeEvent(input$buttonRemoveAllVaccinesSeries,{updateSelectInput(session, 'selectVaccinesForSeries', selected = character(0))})
  observeEvent(input$buttonAddAllDaysSeries,{updateSelectInput(session, 'selectDaysForSeries', selected = vaccinesDaysFromColNames(allData$colNames)[['days']])})
  observeEvent(input$buttonRemoveAllDaysSeries,{updateSelectInput(session, 'selectDaysForSeries', selected = character(0))})
  observeEvent(input$buttonAddAllGenesProbesSeries,{updateSelectInput(session, 'selectGenesProbesForSeries', choices = topGenesAndModules()[['genes']][[genesOrProbes]], selected = topGenesAndModules()[['genes']][[genesOrProbes]])})
  observeEvent(input$buttonRemoveGenesProbesSeries,{updateSelectInput(session, 'selectGenesProbesForSeries', selected = character(0))})
  
  output$buttonSaveTableProbesSeries <- downloadHandler(filename = function(){paste0("Selected Spots-Genes Series.csv")},
     content = function(file) {write.csv(topGenesInSeries, file, row.names = FALSE)})
  

  #################### Genes->Modules #########################
  # output assoc modules
  output$datatableGenesModules <- renderDataTable({
    topGenesAndModules()[['modules']]
  })

  output$buttonSaveTableGenesModules <- downloadHandler(filename = function(){paste0("Selected Genes -> Modules.csv")},
     content = function(file) {write.csv(topGenesAndModules()[['modules']], file, row.names = FALSE)})
  output$buttonSaveTableGenesModulesPlot <- downloadHandler(filename = function(){paste0("Selected Genes -> Modules.png")},
    content = function(file) {plotDataTable(topGenesAndModules()[['modules']],file,35)})
  
  
  #################### Modules #########################
  # get the individual gene values for boxplot and the summ stats of the modules
  geneExpressionsForModules <- reactive({
    getExpressionsForModules(topGenesAndModules(),sortCol_Probes,allData$data, input$checkboxShowPsuedoModuleGenesModules,filtersText())})
  # draw / save table
  output$datatableSelModulesOnly <- renderDataTable({geneExpressionsForModules()[['summStats']]})
  output$buttonSaveTableModulesSummary <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Summary.csv")},
    content = function(file) {write.csv(geneExpressionsForModules()[['summStats']], file, row.names = FALSE)})
  output$buttonSaveTableModulesRaw <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Raw.csv")},
    content = function(file) {write.csv(geneExpressionsForModules()[['expressions']], file, row.names = FALSE)})
  
  output$buttonSaveTableModulesSummaryPlot <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Table.png")},
    content = function(file) {plotDataTable(geneExpressionsForModules()[['summStats']],file,10.9)})
  output$buttonSaveTableModulesSummaryListPlot <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Table.png")},
    content = function(file) {plotDataTable(select(geneExpressionsForModules()[['summStats']],Module,Title),file,20)})
  
  output$buttonSaveTableModulesSummaryList <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes.txt")},
    content = function(file) {write_lines(
      paste0(
      paste(unique(filter(geneExpressionsForModules()[['summStats']], Module != "Selected")[['Module']]), collapse = ','),'\n\n',
      paste(unique(filter(geneExpressionsForModules()[['summStats']], Title != "Selected")[['Title']]), collapse = ','),'\n\n# ',
      dataFilterStr('g')
      ), file)})
  
  
  # draw / save plot
  ggplotGenesModules <-
    reactive({plotGenesModules(geneExpressionsForModules()[['expressions']],dataAndFiltersText(),
                input$checkboxShowLegendGenesModules, input$checkboxShowZeroGenesModules,input$checkboxGGplotGenesModules,
                input$radioGroupProbeModulesBy)})
  output$plotGenesModules <- renderPlot({ggplotGenesModules()} ,res = 72)
  output$plotGenesModulesSIZE <- renderUI({plotOutput("plotGenesModules", height = input$numberPlotGenesModulesSIZEheight)})
  output$buttonPNGplotGenesModules <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes.png")},
    content = function(file) {printPlotPNG(ggplotGenesModules(),file,session$clientData[["output_plotGenesModules_height"]],session$clientData[["output_plotGenesModules_width"]])})
  
  
  #################### Modules->Genes #########################
  # link the module select to the modules for top genes topGenesAndModules()[['modsOnly']]
  mods4Genes <- reactive({moduleDescriptionsForGenes(geneExpressionsForModules()[['summStats']])})
  # change choices in the Genes In Module select based on selected modules
  observeEvent(mods4Genes(),{
    updatePickerInput(session, 'selectModuleForGenes', choices = mods4Genes())
    updateSelectInput(session, 'selectModuleForSeries', choices = mods4Genes())})
  # calculate gene expressions for the module selected
  expressionsInModule <- reactive({getGeneExpressionsInModule(input$selectModuleForGenes,sortCol_Probes,
                                                              allData$data,topGenesAndModules()[['genes']])})
  # redraw the table of gene expressions for the module selected
  output$datatableModuleGenes <- renderDataTable({expressionsInModule()})
  output$buttonTableModulesGenesList <- downloadHandler(filename = function(){paste0("Genes In ",input$selectModuleForGenes,".txt")},
    content = function(file) {write_lines(paste0(paste(rev(unique(expressionsInModule()[['Gene']])), collapse = ','),'\n\n# ',
      input$selectModuleForGenes,'\n\n# ',
      dataFilterStr('g')), file)})
  
  
  
  output$buttonSaveTableModulesGenes <- downloadHandler(filename = function(){paste0("Selected Genes-",input$selectModuleForGenes,"-Genes.csv")},
     content = function(file) {write.csv(expressionsInModule(), file, row.names = FALSE)})
  
  ggplotModuleGenes <- reactive({plotModuleGenes(expressionsInModule(),isolate(input$selectModuleForGenes),
                                dataAndFiltersText(),input$checkboxShowLegendModuleGenes, input$checkboxShowZeroModuleGenes,
                                input$checkboxGGplotModuleGenes, input$checkboxShowMissingModuleGenes)})
  output$plotModuleGenes <- renderPlot({ggplotModuleGenes()} ,res = 72)
  output$plotModuleGenesSIZE <- renderUI({plotOutput("plotModuleGenes", height = input$numberPlotModuleGenesSIZEheight)})
  output$buttonPNGplotModuleGenes <- downloadHandler(filename = function(){paste0("Selected Genes-",input$selectModuleForGenes,".png")},
     content = function(file) {printPlotPNG(ggplotModuleGenes(),file,session$clientData[["output_plotModuleGenes_height"]],session$clientData[["output_plotModuleGenes_width"]])})
  
  #################### Modules Series #########################
  # selectModuleForSeries and selectColumnForModuleSeries are updated above
  assign("moduleValues",NULL, envir = .GlobalEnv)
  assign("ggplotModulesInSeries",NULL, envir = .GlobalEnv)
  observeEvent({
    input$buttonPlotModuleSeries
  },{
    output$plotModuleSeries <- renderPlot({NULL})
    updateplotModuleSeriesBRUSH(list(Table = NULL, Spot = NULL, Gene = NULL))
    
    assign("moduleValues",
           getModuleValuesForSeries(allData$data,
            input$selectModuleForSeries,columnsFromVaccinesDays(input$selectColumnForModuleSeriesVaccines,input$selectColumnForModuleSeriesDays), 
            input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries), 
           envir = .GlobalEnv)
    
    if(!is.null(moduleValues) && input$checkboxShowPseudoModuleModuleSeries == TRUE) {
      assign("moduleValues",
             getTopGenesInSeriesToPlotWithModules(allData$data, topGenesAndModules()[['genes']],
              columnsFromVaccinesDays(input$selectColumnForModuleSeriesVaccines,input$selectColumnForModuleSeriesDays),
              input$checkboxShowFacetModuleSeries,input$radioRibbonBoxModuleSeries,moduleValues), 
             envir = .GlobalEnv)
    }
    
    output$datatableModuleSeries <- renderDataTable({moduleValues})

    assign("ggplotModulesInSeries",
           plotModulesInSeries(moduleValues,dataAndFiltersText(),input$checkboxShowLegendModuleSeries,
                               input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries, input$checkboxShowZeroModuleSeries,
                               input$checkboxShowSEModuleSeries, sortCol_Probes,input$checkboxShowGridModuleSeries, input$checkboxShowPointsModuleSeries,
                               input$numericNumPanelsPlotModuleSeries), 
           envir = .GlobalEnv)
    
    
    output$plotModuleSeries <- renderPlot({ggplotModulesInSeries} ,res = 72)
    output$plotModuleSeriesSIZE <- renderUI({tagList(
      plotOutput("plotModuleSeries", 
                 height = isolate(input$numberPlotModuleSeriesSIZEheight),
                 click = "click_plotModuleSeries", 
                 brush = "brush_plotModuleSeries"
      ))})
    
  })
  output$buttonPNGplotModuleSeries <- downloadHandler(filename = function(){paste0("Selected Genes-Modules Series.png")},
    content = function(file) {printPlotPNG(ggplotModulesInSeries,file,session$clientData[["output_plotModuleSeries_height"]],session$clientData[["output_plotModuleSeries_width"]])})
  
  updateplotModuleSeriesBRUSH <- function(res) {
    output$plotModuleSeriesBRUSH <- renderTable({res$Table}, striped = TRUE)
    output$plotModuleSeriesGENEMOD <- renderText({res$GeneMod})
  }
  
  observeEvent(input$click_plotModuleSeries, 
               {if(input$radioRibbonBoxModuleSeries != 'Boxplot') {
                 res <- handleClick(moduleValues,input$click_plotModuleSeries,input$checkboxShowFacetModuleSeries,FALSE,"Value")
                 if(!is.null(res$Table)) updateplotModuleSeriesBRUSH(res)
               }
  })
  
  observeEvent(input$brush_plotModuleSeries, 
    {if(input$radioRibbonBoxModuleSeries != 'Boxplot') updateplotModuleSeriesBRUSH(handleBrush(moduleValues,input$brush_plotModuleSeries,input$checkboxShowFacetModuleSeries,FALSE,"Value"))
  })
  
  
  output$buttonSaveTableModulesSeries <- downloadHandler(filename = function(){paste0("Selected Genes-Modules Series.csv")},
    content = function(file) {write.csv(moduleValues, file, row.names = FALSE)})
  
  observeEvent(input$buttonAddAllModulesModuleSeries,{updateSelectInput(session, 'selectModuleForSeries', selected = mods4Genes())})
  observeEvent(input$buttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'selectModuleForSeries', selected = character(0))})
  
  observeEvent(input$buttonAddAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'selectColumnForModuleSeriesVaccines', selected = vaccinesDaysFromColNames(allData$colNames)[['vaccines']])})
  observeEvent(input$buttonRemoveAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'selectColumnForModuleSeriesVaccines', selected = character(0))})
  observeEvent(input$buttonAddAllColumnsModuleSeriesDays,{updateSelectInput(session, 'selectColumnForModuleSeriesDays', selected = vaccinesDaysFromColNames(allData$colNames)[['days']])})
  observeEvent(input$buttonRemoveAllColumnsModuleSeriesDays,{updateSelectInput(session, 'selectColumnForModuleSeriesDays', selected = character(0))})
  
  
  
  ############################## Gene Lookup ###########
  assign("lookedupGenes",NULL, envir = .GlobalEnv)
  observeEvent({
    input$buttonGeneLookup
  },{
    assign("lookedupGenes",lookupGenesProbes(input$textInputGeneLookup, allData$annot, input$pickerGeneProbeLookup,input$checkboxGeneLookupWholeWord), envir = .GlobalEnv)
    output$datatableGeneLookup <- renderDataTable({lookedupGenes})
  })
  observeEvent({
    input$buttonGeneLookupNone
  },{
    updateTextInput(session, 'textInputGeneLookup', value = "")
    output$datatableGeneLookup <- renderDataTable({NULL})
  })

  output$buttonSaveTableGeneLookup <- downloadHandler(filename = function(){paste0("Gene Lookup.csv")},
   content = function(file) {write.csv(lookedupGenes, file, row.names = FALSE)})
  
  
  ############################## Network ###########
  assign("vaccDaysToNet",NULL, envir = .GlobalEnv)
  
  observeEvent(
    {
      input$selectVaccNet
    },
    {
      if(is.null(dayPatterns[[input$selectVaccNet]])) {showNotification(type = 'error', ui = paste0("There is a problem with data formatting - no day numbers could be found for ",input$selectVaccNet))}
      updatePickerInput(session, 'selectDayNet', choices = dayPatterns[[input$selectVaccNet]])
    })
  
  observeEvent(
    {
      input$selectVacDaysToNet
    },
    {
      # if we remove an item we must realign the list or it gets added again when we click add
      assign("vaccDaysToNet",input$selectVacDaysToNet, envir = .GlobalEnv)
      updatePickerInput(session, 'selectVacDaysToNet', choices = vaccDaysToNet, selected = vaccDaysToNet)
    })
  
  
  observeEvent(
    {
      input$buttonAddVacDayNet
    },
    {
      vacDy <- paste0(input$selectVaccNet,"_",input$selectDayNet)
      assign("vaccDaysToNet",unique(c(vaccDaysToNet,vacDy)), envir = .GlobalEnv)
      updatePickerInput(session, 'selectVacDaysToNet', choices = vaccDaysToNet, selected = vaccDaysToNet)
    })
  
  observeEvent(
    {
      input$buttonClearNet
    },
    {
      assign("vaccDaysToNet",NULL, envir = .GlobalEnv)
      updatePickerInput(session, 'selectVacDaysToNet', choices = character(0), selected = character(0))
    })
  
  
  numRowsNet <- reactiveVal(NULL)
  networkEdgeListAndCount <- reactiveValues(edgeCount = NULL, edgeList = NULL)
  observeEvent(
    {
      input$buttonPlotNet
    },
    {
      networkEdgeListAndCount$edgeCount <- NULL
      networkEdgeListAndCount$edgeList <- NULL
      networkEListAndCount <- getNetworkEdgeListAndCount(allData$data,input$selectVacDaysToNet,input$numericNumRowsNet,input$checkboxDescNet)
      networkEdgeListAndCount$edgeCount <- networkEListAndCount[['edgeCount']]
      networkEdgeListAndCount$edgeList <- networkEListAndCount[['edgeList']]
      numRowsNet(paste(input$numericNumRowsNet,"rows "))
      updateNumericEdgeThresh()
    })
  
  netFilterString <- reactive({paste(numRowsNet(),
                                    switch(input$radioEdgeCountThreshold,'a' = "All connections ", 'u' = "Unique connections ",'c' = "Common connections ",'v' = paste("Connections >",input$numericEdgeCountThreshold)),
                                    ifelse(input$checkboxThresholdEdgesNet == TRUE,
                                           paste(switch(input$radioLineLabelVariableNet,'MeanValue' = "Value", 'revrank' = "Rank"),"between",input$numericEdgeValueThresholdLo,"and",input$numericEdgeValueThresholdHi),
                                           "")# the F in ifelse
                                    )})
  output$netFilterString <- renderText({netFilterString()})
  
  networkFilteredEdgeListAndCount <- reactive({
    return(getFilteredEdgeListAndEdgeCounts(networkEdgeListAndCount$edgeList,networkEdgeListAndCount$edgeCount,input$radioEdgeCountThreshold,input$numericEdgeCountThreshold,
                                            input$radioLineLabelVariableNet,input$numericEdgeValueThresholdLo,input$numericEdgeValueThresholdHi,input$checkboxThresholdEdgesNet))
  })
  
  output$datatableEdgeListNet <- renderDataTable({
    if(is.null(networkFilteredEdgeListAndCount()[['edgeList']])) NULL
    else select(networkFilteredEdgeListAndCount()[['edgeList']],-c(revrank,MeanValueRound))
  })
  
  output$datatableEdgeCountNet <- renderDataTable({
    if(is.null(networkFilteredEdgeListAndCount()[['edgeCount']])) NULL
    else arrange(networkFilteredEdgeListAndCount()[['edgeCount']],desc(Connections))
  })
  
  vennVaccGenesList <- reactive({getVennVaccGenesList(networkFilteredEdgeListAndCount()[['edgeList']], isolate(input$selectVacDaysToNet))})
  
  output$datatableIntersectsNet <- renderDataTable({
    geneIntersectsFromVaccGenesList(vennVaccGenesList())
  })
  
  
  output$plotVenn <- renderPlot({
    if(is.null(vennVaccGenesList())) NULL
    else grid.draw(venDiagramFromVaccGenesList(vennVaccGenesList()))
  })
  output$plotEuler <- renderPlot({
    eulerFromVaccGenesList(vennVaccGenesList())
  })
  output$plotUpset <- renderPlot({
    upsetrFromVaccGenesList(vennVaccGenesList())
  })
  
  #input$plotNetSIZEheight below is to just react
  networkQgraph <- reactive({getNetworkQgraph(networkFilteredEdgeListAndCount(),input$radioNetType, 
      input$radioLineLabelVariableNet, input$checkboxLineLabelsNet,input$nodeAlphaNet)})
  output$plotNet <- renderPlot({
    if(!is.null(networkQgraph())) plot(networkQgraph())
    else NULL
  })
  
  output$plotNetSIZE <- renderUI({plotOutput("plotNet", height = input$plotNetSIZEheight)})
  output$plotVennSIZE <- renderUI({plotOutput("plotVenn", height = input$plotNetSIZEheight)})
  output$plotEulerSIZE <- renderUI({plotOutput("plotEuler", height = input$plotNetSIZEheight)})
  output$plotUpsetSIZE <- renderUI({plotOutput("plotUpset", height = input$plotNetSIZEheight)})
  
  output$buttonPNGNet <- downloadHandler(filename = function(){
    paste0(
    switch(input$radioVennNetworkeNet,"n" = "Network ","e" = "Euler ","v" = "Venn ","u" = "UpSetR "),
    paste(input$selectVacDaysToNet)," ",
    netFilterString(),
    ".png"
    )},
    content = function(file) {
      # upsetR wants to draw directly and returns nothing
      if(input$radioVennNetworkeNet == "u") {
        upSetRPNG(vennVaccGenesList(),file,session$clientData[["output_plotNet_height"]],session$clientData[["output_plotNet_width"]])
      }
      else plotPlotPNG(
      switch(input$radioVennNetworkeNet,
             "n" = networkQgraph(),
             "e" = eulerFromVaccGenesList(vennVaccGenesList()),
             "v" = venDiagramFromVaccGenesList(vennVaccGenesList())
             ),
      file,session$clientData[["output_plotNet_height"]],session$clientData[["output_plotNet_width"]],
      input$radioVennNetworkeNet)
      })
  
  updateNumericEdgeThresh <- function() {
    minmax <- getEdgeMinMax(networkEdgeListAndCount$edgeList,input$radioLineLabelVariableNet)
    updateNumericInput(session,'numericEdgeValueThresholdLo',value = minmax[['Min']])
    updateNumericInput(session,'numericEdgeValueThresholdHi',value = minmax[['Max']])
  }
  observeEvent(
    {
      networkEdgeListAndCount$edgeList
      input$radioLineLabelVariableNet
      input$buttonResetEdgeLimitNumericsNet
    },
    {
      updateNumericEdgeThresh()
    }, ignoreInit = TRUE)
  
#################### MODULES ####################  
  # sortCol_Mods <- NULL
  assign("sortCol_Mods",NULL, envir = .GlobalEnv)
  
  #################### Selecting Modules ####

  respondToChangeColumnModules <- function(){
    assign(
      "sortCol_Mods",
      columnsFromVaccinesDays(input$mselectColumnVaccine, input$mselectColumnDay),
      envir = .GlobalEnv
    )
    updateModuleMinMax(sortCol_Mods)
  }
  
  observeEvent({
    input$mselectColumnDay
  },
  {
    respondToChangeColumnModules()
  })
  
  observeEvent({
    input$mselectColumnVaccine
  },
  {
    if(is.null(dayPatterns[[input$mselectColumnVaccine]])) {showNotification(type = 'error', ui = paste0("There is a problem with data formatting - no day numbers could be found for ",input$mselectColumnVaccine))}
    updatePickerInput(session, 'mselectColumnDay', choices = dayPatterns[[input$mselectColumnVaccine]])
    respondToChangeColumnModules()
  })
  
  
observeEvent(
  input$mbuttonResetValuesRangeCol,
  {
    updateModuleMinMax(sortCol_Mods)})

observeEvent(
  {
    input$mbuttonResetValuesRangeData
    input$mcheckboxModuleMedians
  },
  {
    updateModuleMinMax(allData$colNames)})

topModulesSelected <- reactiveVal()
topModulesSelected(NULL)


observeEvent(
  input$mbuttonApplySelection,
  {
    {
      if(!is.null(sortCol_Mods)) {
        # calculate topGenesAndModules()
        if(input$mcheckboxSelectKeyword == FALSE && input$mcheckboxSelectValues == FALSE && input$mcheckboxSelectRows == FALSE) {
          showModal(modalDialog(
            title = "Too Many Modules","You must have at least one filter selected or it will try to return and plot over 600 modules."))
        } else {
          showNotification("Please wait for filters to be applied…", id = "mbuttonApplySelection", type = 'message', duration = 3)
          # show tabs as we have selected modules
          showTab(inputId = "navModule", target = "Selected Modules")
          showTab(inputId = "navModule", target = "Module->Genes")
          showTab(inputId = "navModule", target = "Modules:Series")
          
          assign("sortCol_Mods",columnsFromVaccinesDays(input$mselectColumnVaccine, input$mselectColumnDay), envir = .GlobalEnv)
          
          filterSubText <-  ""
          # apply the filters sequentially
          
          if(input$mcheckboxSelectKeyword == TRUE){
            mods <- getModulesForSearch(allData$modulesMeans,input$mtextInputKeyword,input$mradioKeywordColumn, input$mcheckboxModuleSearchWholeWord)
            if(dataFrameOK(mods)) {
              filterSubText <-  paste0(filterSubText,'"',input$mtextInputKeyword,'" in ',input$mradioKeywordColumn,' ')
              mods <- getSortedModulesForVaccDay(mods,sortCol_Mods,input$mcheckboxDescending,input$mcheckboxModuleMedians)
            }
          } else {
            mods <- getSortedModulesForVaccDay(allData$modulesMeans,sortCol_Mods,input$mcheckboxDescending,input$mcheckboxModuleMedians)
          }
          
          
          if(input$mcheckboxSelectValues == TRUE && dataFrameOK(mods)){
            mods <- getModulesForValues(mods,input$mnumberExpressionMin,input$mnumberExpressionMax,input$mcheckboxModuleMedians)
            filterSubText <-  paste0(filterSubText,'Value from ',input$mnumberExpressionMin,' to ',input$mnumberExpressionMax,' ')
          }
          if(input$mcheckboxSelectRows == TRUE && dataFrameOK(mods)){
            mods <- getModulesForRows(mods,input$mnumberModsStart,input$mnumberModsEnd)
            filterSubText <-  paste0(filterSubText,'Rows from ',input$mnumberModsStart,' to ',input$mnumberModsEnd,' ')
          }
      
          if(dataFrameOK(mods)) {
            if(nchar(filterSubText) > 0) {
              modulesAndFiltersText(
                paste0(sortCol_Mods,' ',filterSubText,' ',
                       ifelse(input$mcheckboxDescending == TRUE, ' Sort Descending ',' Sort Ascending '),
                       ifelse(input$mcheckboxModuleMedians == TRUE, ' Use Median ',' Use Mean ')
                ))
            } else {
              modulesAndFiltersText(
                paste0(sortCol_Mods,' [No filters] ',
                       ifelse(input$mcheckboxDescending == TRUE, ' Sort Descending, ',' Sort Ascending, '),
                       ifelse(input$mcheckboxModuleMedians == TRUE, ' Use Median ',' Use Mean ')
                ))
            }
            # ASSIGN topModulesSelected
            topModulesSelected(mods)
            show(id = "navModuleHeader")
            removeNotification(id = "mbuttonApplySelection")
            showNotification(paste0("Found: ",length(unique(mods[["Module"]])), " modules"), type = 'message')
            
          } else {
            modulesAndFiltersText("")
            topModulesSelected(NULL)
            hide(id = "navModuleHeader")
            removeNotification(id = "mbuttonApplySelection")
            showNotification(paste0("Found: 0 modules"), type = 'warning')
          }
        }
      } else {
        showNotification("A column to sort must always be selected, even if just filtering by regex", type = 'error')
      }
    }
  }
)


  #################### Selected Modules #########################
observeEvent(
  topModulesSelected(),
  {
    # these are non-reactive and need a manual reboot
    output$mplotModuleSeries <- renderPlot({NULL})
    output$mdatatableModuleSeries <- renderDataTable({NULL})
    updatemplotModuleSeriesBRUSH(list(Table = NULL, Spot = NULL, Gene = NULL))
    
    updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', selected = character(0))
    updateSelectInput(session, 'mselectColumnForModuleSeriesDays', selected = character(0))
    updateSelectInput(session, 'mselectPlotModulesInSeries',choices = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))
    updatePickerInput(session, 'mselectModuleForGenes',choices = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))
    
  }
)

# output top genes
output$mdatatableTopModulesUp <- renderDataTable({topModulesSelected()})
output$buttonSaveTableTopModulesUpPlot <- downloadHandler(filename = function(){paste0("Selected By Modules.png")},
  content = function(file) {plotDataTable(topModulesSelected(),file,10.9)})
output$buttonSaveTableTopModulesUOnlypPlot <- downloadHandler(filename = function(){paste0("Selected By Modules.png")},
  content = function(file) {plotDataTable(select(topModulesSelected(),Rank:Category),file,10.9)})

output$mbuttonSaveTableModules <- downloadHandler(filename = function(){paste0("Selected By Modules.csv")},
  content = function(file) {write.csv(topModulesSelected(), file, row.names = FALSE)})


output$mbuttonSaveListTopModules <- downloadHandler(filename = function(){paste0("Selected By Modules-",input$pickerSaveListTopModules,".txt")},
  content = function(file) {write_lines(paste0(paste(unique(topModulesSelected()[[input$pickerSaveListTopModules]]), collapse = ','),'\n\n# ',dataFilterStr('m')), file)})

  # Plot Modules Selected #
ggplotSelectedModules <-
  reactive({plotSelectedModules(allData$modules,topModulesSelected(),modulesAndFiltersText(), 
    input$mcheckboxShowLegendGenesModules, input$mcheckboxShowZeroGenesModules,input$mcheckboxModuleMedians,input$mradioGroupTitleName,input$mcheckboxGGplotGenesModules)})
output$mplotSelectedModules <- renderPlot({ggplotSelectedModules()} ,res = 72)
output$mplotSelectedModulesSIZE <- renderUI({plotOutput("mplotSelectedModules", height = input$numbermplotSelectedModulesSIZEheight)})
output$buttonPNGmplotSelectedModules <- downloadHandler(filename = function(){paste0("Selected Modules.png")},
  content = function(file) {printPlotPNG(ggplotSelectedModules(),file,session$clientData[["output_mplotSelectedModules_height"]],session$clientData[["output_mplotSelectedModules_width"]])})

#################### Modules Modules->Genes #########################

# change choices in the Genes In Module select based on selected modules
# observeEvent(topModulesSelected() is above and resets our menu
# calculate gene expressions for the module selected
expressionsInModuleModule <- reactive({getGeneExpressionsInModule(input$mselectModuleForGenes,sortCol_Mods,allData$data,NULL)})
# # redraw the table of gene expressions for the module selected
output$mdatatableModuleGenes <- renderDataTable({select(expressionsInModuleModule(),-Selected)})
output$buttonSaveTableModulesGenes <- downloadHandler(filename = function(){paste0("Genes in ",input$mselectModuleForGenes,".csv")},
                                                      content = function(file) {write.csv(expressionsInModuleModule(), file, row.names = FALSE)})

output$mbuttonTopModulesGenesList <- downloadHandler(filename = function(){paste0("Genes In ",input$mselectModuleForGenes,".txt")},
  content = function(file) {write_lines(paste0(paste(rev(unique(expressionsInModuleModule()[['Gene']])), collapse = ','),'\n\n# ',
                                               input$mselectModuleForGenes,'\n\n# ',
                                               dataFilterStr('m')), file)})



ggplotModuleModuleGenes <- reactive({plotModuleGenes(expressionsInModuleModule(),isolate(input$mselectModuleForGenes),
                                                    modulesAndFiltersText(),input$mcheckboxShowLegendModuleGenes, input$mcheckboxShowZeroModuleGenes,
                                                    input$mcheckboxGGplotModuleGenes, input$mcheckboxShowMissingModuleGenes)})
output$mplotModuleGenes <- renderPlot({ggplotModuleModuleGenes()} ,res = 72)
output$mplotModuleGenesSIZE <- renderUI({plotOutput("mplotModuleGenes", height = input$mnumberPlotModuleGenesSIZEheight)})
output$mbuttonPNGplotModuleGenes <- downloadHandler(filename = function(){paste0("Genes in ",input$mselectModuleForGenes,".png")},
 content = function(file) {printPlotPNG(ggplotModuleModuleGenes(),file,session$clientData[["output_mplotModuleGenes_height"]],session$clientData[["output_mplotModuleGenes_width"]])})

  #################### Plot Modules Selected Series #########################

# ggplotSelectedModulesSeries <- NULL
assign("ggplotSelectedModulesSeries",NULL, envir = .GlobalEnv)

observeEvent({
  input$mbuttonPlotModuleSeries
},{
    mods2plot <- NULL
    if(input$radioModulesModulesSeries == 'Filters') {mods2plot <- input$mselectPlotModulesInSeries}
    else if(input$radioModulesModulesSeries == 'Titles') {mods2plot <- getModulesForTitles(input$mselectModuleTitles,allData$modulesMeans)}
    else if(input$radioModulesModulesSeries == 'Modules') {mods2plot <- input$mselectModuleAllModules}

    assign("ggplotSelectedModulesSeries",
           plotSelectedModulesSeries(allData,
                                     columnsFromVaccinesDays(input$mselectColumnForModuleSeriesVaccines,input$mselectColumnForModuleSeriesDays),
                                     mods2plot,modulesAndFiltersText(),input$mcheckboxShowLegendModuleSeries,
                                     input$mcheckboxShowZeroModuleSeries,input$mradioRibbonBoxModuleSeries, input$mcheckboxShowFacetModuleSeries,
                                     input$mcheckboxShowSEModuleSeries, input$mradioGroupTitleNameModuleSeries, input$mcheckboxShowGridSeries,
                                     input$mcheckboxShowPointsSeries,sortCol_Mods, input$numericNumPanelsmplotModuleSeries),
           envir = .GlobalEnv)
    
    
    output$mplotModuleSeries <- renderPlot({ggplotSelectedModulesSeries[['plot']]} ,res = 72)
    output$mplotModuleSeriesSIZE <- renderUI({tagList(
                                                      plotOutput("mplotModuleSeries", 
                                                      height = isolate(input$numbermplotModuleSeriesSIZEheight),
                                                      click = "click_mplotModuleSeries",
                                                      brush = "brush_mplotModuleSeries"
                                                      ))})
    output$mdatatableModuleSeries <- renderDataTable({ggplotSelectedModulesSeries[['table']]})
    updatemplotModuleSeriesBRUSH(list(Table = NULL, Spot = NULL, Gene = NULL))
    
})
output$buttonPNGmplotModuleSeries <- downloadHandler(filename = function(){paste0("Selected Modules Series.png")},
  content = function(file) {printPlotPNG(ggplotSelectedModulesSeries[['plot']],file,session$clientData[["output_mplotModuleSeries_height"]],session$clientData[["output_mplotModuleSeries_width"]])})

updatemplotModuleSeriesBRUSH <- function(res) {
  output$mplotModuleSeriesBRUSH <- renderTable({res$Table}, striped = TRUE)
  output$mplotModuleSeriesGENEMOD <- renderText({res$GeneMod})
}

observeEvent(input$click_mplotModuleSeries, 
             {if(input$mradioRibbonBoxModuleSeries != 'Boxplot') {
               res <- handleClick(ggplotSelectedModulesSeries[['table']],input$click_mplotModuleSeries,input$mcheckboxShowFacetModuleSeries,FALSE,"Mean")
               if(!is.null(res$Table)) updatemplotModuleSeriesBRUSH(res)
               }
})

observeEvent(input$brush_mplotModuleSeries, 
             {if(input$mradioRibbonBoxModuleSeries != 'Boxplot') {
               updatemplotModuleSeriesBRUSH(handleBrush(ggplotSelectedModulesSeries[['table']],input$brush_mplotModuleSeries,input$mcheckboxShowFacetModuleSeries,FALSE,"Mean"))
             }
})

observeEvent(input$mbuttonAddAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', selected = vaccinesDaysFromColNames(allData$colNames)[['vaccines']])})
observeEvent(input$mbuttonRemoveAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', selected = character(0))})
observeEvent(input$mbuttonAddAllColumnsModuleSeriesDays,{updateSelectInput(session, 'mselectColumnForModuleSeriesDays', selected = vaccinesDaysFromColNames(allData$colNames)[['days']])})
observeEvent(input$mbuttonRemoveAllColumnsModuleSeriesDays,{updateSelectInput(session, 'mselectColumnForModuleSeriesDays', selected = character(0))})


observeEvent(input$mbuttonAddAllModuleSeries,{updateSelectInput(session, 'mselectPlotModulesInSeries', selected = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))})
observeEvent(input$mbuttonRemoveAllModuleSeries,{updateSelectInput(session, 'mselectPlotModulesInSeries',selected = character(0))})

observeEvent(input$mbuttonRemoveAllModuleTitles,{updateSelectInput(session, 'mselectModuleTitles', selected = character(0))})

observeEvent(input$mbuttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleAllModules', selected = character(0))})


output$mbuttonSaveTableModulesSeries <- downloadHandler(filename = function(){paste0("Selected By Modules Series.csv")},
  content = function(file) {write.csv(ggplotSelectedModulesSeries[['table']], file, row.names = FALSE)})

output$mbuttonSaveListTopModuleTitlesSeries <- downloadHandler(filename = function(){paste0("Selected By Modules Titles For Series.txt")},
 content = function(file) {write_lines(paste0(paste(unique(input$mselectModuleTitles), collapse = ','),'\n\n# ',dataFilterStr('m')), file)})
output$mbuttonSaveListTopModulesSeries <- downloadHandler(filename = function(){paste0("Selected By Modules For Series.txt")},
  content = function(file) {write_lines(paste0(paste(unique(modNameFromMenuTitle(input$mselectModuleAllModules)), collapse = ','),'\n\n# ',dataFilterStr('m')), file)})


#################### Modules Lookup #########################
# lookedupMods <- NULL
assign("lookedupMods",NULL, envir = .GlobalEnv)

observeEvent({
  input$mbuttonModLookup
  input$radioArrangeModuleLookupBy
},{
  # use <<-
  # lookedupMods <<- lookupModules(input$mtextInputModLookup, allData$modulesMeans,input$radioArrangeModuleLookupBy)
  assign("lookedupMods",lookupModules(input$mtextInputModLookup, allData$modulesMeans,input$radioArrangeModuleLookupBy,input$mcheckboxModuleLookupWholeWord), envir = .GlobalEnv)

    output$mdatatableModuleLookup <- renderDataTable({lookedupMods})
})

observeEvent({
  input$mbuttonModLookupNone
},{
  updateTextInput(session, 'mtextInputModLookup', value = "")
  output$mdatatableModuleLookup <- renderDataTable({NULL})
})

output$mbuttonSaveTableModuleLookup <- downloadHandler(filename = function(){paste0("Module Lookup.csv")},
  content = function(file) {write.csv(lookedupMods, file, row.names = FALSE)})

#################### Cells #########################
assign("cellsData",NULL, envir = .GlobalEnv)
assign("cellsColours",NULL, envir = .GlobalEnv)
assign("vaccineShapes",NULL, envir = .GlobalEnv)
assign("vaccineNames",NULL, envir = .GlobalEnv)
assign("cellTypesInData",NULL, envir = .GlobalEnv)

observeEvent(input$buttonLoadCells, {
  cellsdataPath <- "cellsdata.rds"
  if (file.exists(cellsdataPath)) {
    assign("cellsData",read_rds(cellsdataPath), envir = .GlobalEnv)
    if(!is.null(cellsData)) {
      assign("cellTypesInData",unique(cellsData[["Mean"]][["Cells"]]), envir = .GlobalEnv)
      assign("vaccineNames",unique(cellsData[["Mean"]][["Treatment"]]), envir = .GlobalEnv)
      allCols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
      assign("cellsColours",set_names(allCols[1:length(cellTypesInData)], cellTypesInData), envir = .GlobalEnv)
      # 12 vaccines but be flexible, if 12 then use predefined shapes, or if less a subset of those, or if more then a sequence
      assign("vaccineShapes",set_names(c(15:18,21:25,0:2), vaccineNames), envir = .GlobalEnv)
      updateSelectInput(session, "selectColumnForCellsSeriesVaccines", choices = unique(cellsData$Mean$Treatment), selected = character(0))
      updateSelectInput(session, "selectColumnForCellsSeriesDays", choices = unique(cellsData$Mean$Day), selected = character(0))
      updateSelectInput(session, "selectCellsForSeries", choices = unique(cellsData$Mean$Cells), selected = character(0))

      showNotification("White Blood Cells Data File Loaded")

      hide("divLoadCells")
      show("divCells", anim = TRUE)
    } else {
      showNotification("White Blood Cells Data File Cannot Be Loaded", type = "error")
    }
  } else {
    showNotification("No White Blood Cells Data File Found", type = "error")
  }
})

observeEvent(input$buttonAddAllColumnsCellsSeriesVaccines,{updateSelectInput(session, 'selectColumnForCellsSeriesVaccines', selected = unique(cellsData$Mean$Treatment))})
observeEvent(input$buttonRemoveAllColumnsCellsSeriesVaccines,{updateSelectInput(session, 'selectColumnForCellsSeriesVaccines', selected = character(0))})
observeEvent(input$buttonAddAllColumnsCellsSeriesDays,{updateSelectInput(session, 'selectColumnForCellsSeriesDays', selected = unique(cellsData$Mean$Day))})
observeEvent(input$buttonRemoveAllColumnsCellsSeriesDays,{updateSelectInput(session, 'selectColumnForCellsSeriesDays', selected = character(0))})
observeEvent(input$buttonAddAllCellsCellsSeries,{updateSelectInput(session, 'selectCellsForSeries', selected = unique(cellsData$Mean$Cells))})
observeEvent(input$buttonRemoveAllCellsCellsSeries,{updateSelectInput(session, 'selectCellsForSeries',selected = character(0))})

observeEvent({
  input$buttonPlotCellsSeries
},{

  assign("ggplotSelectedCellsSeries",
         plotSelectedCellsSeries(cellsData,input$radioMeanFCCellsSeries,
                                input$selectColumnForCellsSeriesVaccines,input$selectColumnForCellsSeriesDays,input$selectCellsForSeries,
                                input$radioRibbonBoxCellsSeries, allData$folder,
                                input$checkboxShowLegendSumCellsSeries,input$checkboxShowLegendAllCellsSeries,input$checkboxShowZeroCellsSeries,
                                input$checkboxShowFacetCellsSeries,input$checkboxShowFacetVaccsSeries,
                                input$radioCellsErrorType, input$checkboxShowGridCellsSeries, input$checkboxShowPointsCellsSeries,
                                input$checkboxFreeYCellsSeries,input$numericNumPanelsCellsSeries, input$radioColoursVaccineCells),
         envir = .GlobalEnv)
  
  
  output$plotCellsSeries <- renderPlot({ggplotSelectedCellsSeries[['plot']]} ,res = 72)
  output$plotCellsSeriesSIZE <- renderUI({plotOutput("plotCellsSeries", height = isolate(input$numericPlotCellSeriesSIZEheight))})

  output$datatableCellsSeries <- renderDataTable({ggplotSelectedCellsSeries[['table']]})
  
})

output$buttonPNGplotCellsSeries <- downloadHandler(filename = function(){paste0("Cell Kinetics.png")},
  content = function(file) {printPlotPNG(ggplotSelectedCellsSeries[['plot']],file,session$clientData[["output_plotCellsSeries_height"]],session$clientData[["output_plotCellsSeries_width"]])})

output$buttonSaveTableCellsSeries <- downloadHandler(filename = function(){paste0("Cell Kinetics.csv")},
  content = function(file) {write.csv(ggplotSelectedCellsSeries[['table']], file, row.names = FALSE)})

#################### Cytokines #########################

cytokinesDataAndPlot <- reactiveValues(data = NULL, plot = NULL)
assign("cytokines",NULL, envir = .GlobalEnv)

observeEvent(input$buttonLoadCytokines, {
    #   load cytokines and update
    cyts <- list(`Fold Increase` = read_rds("cytFoldT.rds"), Concentration = read_rds("cytokinesT.rds"))
    if(!is.null(cyts)) {
      assign("cytokines",cyts, envir = .GlobalEnv)
      updateSelectInput(session, "cselectCytokines", choices = sort(unique(cyts[["Fold Increase"]][["CYTOKINE"]])))
      updateSelectInput(session, "cselectTreatments", choices = sort(unique(cyts[["Fold Increase"]][["ACTARMCD"]])))
      updateSelectInput(session, "cselectDays", choices = sort(unique(cyts[["Fold Increase"]][["DAY"]])))
      
      showNotification("Cytokines Data File Loaded")
      
      hide("divLoadCytokines")
      show("divCytokines", anim = TRUE)
    } else {
      showNotification("Cytokines Data File Cannot Be Loaded", type = "error")
    }
})

observeEvent(input$buttonPlotCytokines, {
  cdp <- getCytokinesDataAndPlot(cytokines[[input$cradioCytoMeansRaw]], input$cselectCytokines,
    input$cselectDays, input$cselectTreatments,input$cradioCytokinesWrap,
    input$cradioCytokinesPlotType,input$cradioCytokinesErrorType, input$ccheckboxZoomQuantile, input$ccheckboxFixedY,
    input$ccheckboxOmit0, input$ccheckboxShowN,input$cnumericNumPanels,input$cradioCytoMeansRaw,
    input$ccheckboxShowPoints, input$cradioCytokinesTransformY, input$ccheckboxLogMeans,
    (input$ccheckboxShow1 & input$cradioCytoMeansRaw == 'Fold Increase'),# calc if we plot --- at 1
    input$ccheckboxMonochrome)
  cytokinesDataAndPlot$data <- cdp$data
  cytokinesDataAndPlot$plot <- cdp$plot
    
  output$cdatatableCytokines <- renderDataTable({cdp$data})
  output$cplotCytokines <- renderPlot({cdp$plot} ,res = 72)
  output$cplotCytokinesSIZE <- renderUI({plotOutput("cplotCytokines", height = isolate(input$cnumberPlotCytokinesSIZEheight))})
  
})
output$cbuttonPNGplotCytokines <- downloadHandler(filename = function(){paste0("Selected Cytokines.png")},
  content = function(file) {printPlotPNG(cytokinesDataAndPlot$plot,file,session$clientData[["output_cplotCytokines_height"]],session$clientData[["output_cplotCytokines_width"]])})

output$buttonSaveTableCytokines <- downloadHandler(filename = function(){paste0("Selected Cytokines.csv")},
  content = function(file) {write.csv(cytokinesDataAndPlot$data, file, row.names = FALSE)})

observeEvent(input$cbuttonAddAllCytokines,{updateSelectInput(session, 'cselectCytokines', selected = sort(unique(cytokines[["Fold Increase"]][["CYTOKINE"]])))})
observeEvent(input$cbuttonAddNoneCytokines,{updateSelectInput(session, 'cselectCytokines', selected = character(0))})
observeEvent(input$cbuttonAddAllCytokineTreats,{updateSelectInput(session, 'cselectTreatments', selected = sort(unique(cytokines[["Fold Increase"]][["ACTARMCD"]])))})
observeEvent(input$cbuttonAddNoneCytokineTreats,{updateSelectInput(session, 'cselectTreatments', selected = character(0))})
observeEvent(input$cbuttonAddAllCytokineDays,{updateSelectInput(session, 'cselectDays', selected = sort(unique(cytokines[["Fold Increase"]][["DAY"]])))})
observeEvent(input$cbuttonAddNoneCytokineDays,{updateSelectInput(session, 'cselectDays', selected = character(0))})


  #################### End Of Server #########################

show("hiddenDiv")

} # end of server
