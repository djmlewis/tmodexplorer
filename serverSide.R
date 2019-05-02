
server <- function(input, output, session) {
  
#   #################### Initial Setup #########################
  is_local <- Sys.getenv('SHINY_PORT') == ""
  # initial hidden setup
  # hide the explores until load
  hideTab(inputId = "navbarTop", target = "Explore By Gene")
  hideTab(inputId = "navbarTop", target = "Network Genes")
  hideTab(inputId = "navbarTop", target = "Explore By Module")
  hideTab(inputId = "navbarTop", target = "Lookup")

    if(is_local == FALSE) {
      hideTab(inputId = "navbarTop", target = "Transcriptomics")
      hideTab(inputId = "navbarTop", target = "ReadMe")
      hideTab(inputId = "navbarTop", target = "Cells")
      hideTab(inputId = "navbarTop", target = "Cytokines")
    } else {
    hideTab(inputId = "navbarTop", target = "Password")
  }
  
  
  assign("selectedVaccineDayFromSavedSearchGene",NULL, envir = .GlobalEnv)
  assign("sortCol_Probes",NULL, envir = .GlobalEnv)
  assign("expressionValueRangeVaccDay",list(Max = 0, Min = 0), envir = .GlobalEnv)
  expressionValueRangeVaccAllDays <- reactiveVal(list(Max = 0, Min = 0))
  assign("dataValueRange",expressionValueRangeVaccDay, envir = .GlobalEnv)
  shapeKinetics <- reactiveVal(NULL)
  
  # assign("dayPatterns",read_rds("dayPats.rds"), envir = .GlobalEnv)
  assign("vaccineColours",read_rds("vaccinecolours.rds"), envir = .GlobalEnv)
  
  filteredSortedProbesTidyMuscle <- reactiveVal(NULL)
  tissueVaccineHourFilteredMuscle <- reactiveVal(NULL)
  
#   #################### Password #########################
  password <- read_rds("p")
  observeEvent(input$buttonPassword, {
    if (input$password == password) {
      hideTab(inputId = "navbarTop", target = "Password")
      showTab(inputId = "navbarTop", target = "Transcriptomics", select = TRUE)
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

  allData <-
    reactiveValues(
      data = NULL,
      colNames = NULL,
      folder = NULL,
      folderpath = NULL,
      modules = NULL,
      modulesMeans = NULL,
      muscleIndividuals = NULL,
      goData = NULL
    )
  
  observeEvent(allData$data, {
    removeNotification(id = "dataLoadingNotification",session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$buttonLoadDataFI, {if (getNewData(allData,input$selectDataFI) == TRUE) {updateLoadControls()}},ignoreInit = TRUE)

  output$buttonsavedatatableAll <- downloadHandler(filename = function(){paste0(allData$folder,".csv")},
    content = function(file) {write_excel_csv(allData$data, file, na = "")})
  
  output$datatableAll <- renderDataTable({allData$data},options = list(searching = TRUE,pageLength = 10))
  
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
    }
  }
  
  # KINETICS MATCHING ###############
  
  vaccineHasThisDay <- function(vax,day){
    return(paste0(vax,"_",day) %in% allData$colNames)
  }
  
  assign("copiedDayKinetics",NULL, envir = .GlobalEnv)
  
  output$vaccineHasDay <- reactive({vaccineHasThisDay(input$selectColumnVaccine,input$selectShapeDay)})#paste0(input$selectColumnVaccine,"_",input$selectShapeDay) %in% allData$colNames})
  # to access vaccineHasDay in GUI we need to force it to recalculate even though its not rendered in the GUI
  outputOptions(output, "vaccineHasDay", suspendWhenHidden = FALSE)
  output$textVaccineHasDay <- renderText({paste(input$selectShapeDay,"not available for",input$selectColumnVaccine)})

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
                        {tibble(Min = data[["Min"]],
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
  
  makeNewKineticsFromFile <- function(kinsfile){
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
    updateDayKineticsToDF(defkins[[input$selectShapeDay]])
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
      assign("copiedDayKinetics",tibble(Exclude = input$checkboxShapeSkipDay ,Min = input$numberShapeDayMin, Max = input$numberShapeDayMax), envir = .GlobalEnv)
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
      curKinetics[[day]] <- tibble(Min = dataValueRange[["Min"]],Max = dataValueRange[["Max"]], Exclude = TRUE)
      if(kineticsNotAllExcluded(curKinetics)) {
        resetShapeNumericsToDataset()
        shapeKinetics(curKinetics)
      } else {
        showNotification("This window cannot be excluded as at least one window must be defined", type = 'error')
      }
    } else {
      curKinetics[[day]] <- tibble(Min = minVal,Max = maxVal, Exclude = ignore)
      shapeKinetics(curKinetics)
    }
  }
  
  observeEvent(
    input$click_plotShapeMiniplot, 
    {
      df <-  kineticsDF(shapeKinetics()) %>%
        mutate(DayF = 1:length(Day))
      res <- nearPoints(df, input$click_plotShapeMiniplot, xvar = "DayF", yvar = "Y", maxpoints = 1,threshold = 10) 
      if(nrow(res)>0 && vaccineHasThisDay(input$selectColumnVaccine,res$Day[1])){ #paste0(input$selectColumnVaccine,"_",res$Day[1]) %in% allData$colNames) {
        updatePickerInput(session,"selectShapeDay", selected = as.character(res$Day[1]))
      }
    },ignoreInit = TRUE,ignoreNULL = TRUE)
  
  observeEvent(
    input$dblclick_plotShapeMiniplot, 
    {
      df <-  kineticsDF(shapeKinetics()) %>%
        mutate(DayF = 1:length(Day))
      res <- nearPoints(df, input$dblclick_plotShapeMiniplot, xvar = "DayF", yvar = "Y", maxpoints = 1,threshold = 10) 
      if(nrow(res)>0 && vaccineHasThisDay(input$selectColumnVaccine,res$Day[1])){ #paste0(input$selectColumnVaccine,"_",res$Day[1]) %in% allData$colNames) {
        # dayF is an index 1...ndays. Use that to lookup the index of day that has the Day value
        selday <- as.character(res$Day[1])
        selKinDF <- shapeKinetics()[[selday]]
        saveKineticsDayValues(!selKinDF$Exclude[1],selday,dataValueRange[["Min"]],dataValueRange[["Max"]])
        updatePickerInput(session,"selectShapeDay", selected = selday)
        updateDayKineticsToDF(shapeKinetics()[[selday]])
      }
    },ignoreInit = TRUE,ignoreNULL = TRUE)
  
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
         makeNewKineticsFromFile(kinsfile)
         #input$buttonLoadShapeKinetics$name is the filename
         # if(identical(defnames,names(newkins))) showNotification(paste0("Imported shape kinetics from ",input$buttonLoadShapeKinetics$name),type = "message")
         # else showNotification(paste0("Imported shape kinetics from ",input$buttonLoadShapeKinetics$name," - however they seem to be from a different dataset and so may be incorrect"),type = "warning")
       } else {showNotification(paste0(input$buttonLoadShapeKinetics$name, " appears not to contain valid kinetics"),type = "error")}
     } else {showNotification(paste0("There was noting to import from ",input$buttonLoadShapeKinetics$name),type = "error")}
   })
  
  
  ggPlotShapeMiniplot <- reactive({getGGplotShapeMiniplot(shapeKinetics(),expressionValueRangeVaccAllDays(),input$selectColumnVaccine,colnames(allData$data))})
  output$plotShapeMiniplot <- renderPlot({ggPlotShapeMiniplot()})
  
  
  # LOADING ##########
  
  updateLoadControls <- function(){
    
    # show hide the nav tabs to reflect we have loaded data, rehide any needing rehiding post select
    showTab(inputId = "navbarTop", target = "Explore By Gene")
    showTab(inputId = "navbarTop", target = "Network Genes")
    showTab(inputId = "navbarTop", target = "Explore By Module")
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
    assign("dayPatterns",vaxDayPatterns(allData$colNames), envir = .GlobalEnv)
    
    vaccDays <- vaccinesDaysFromColNames(allData$colNames)
    updatePickerInput(session, 'selectColumnVaccine', choices = vaccDays$vaccines)
    updatePickerInput(session, 'go_selectColumnVaccine', choices = vaccDays$vaccines)
    updatePickerInput(session, 'selectVaccNet', choices = vaccDays$vaccines)
    
    updatePickerInput(session, 'selectVacDaysToNet', choices = character(0), selected = character(0))
    
    
    updateSelectInput(session, 'selectVaccinesForSeries', choices = vaccDays$vaccines, selected = character(0))
    updateSelectInput(session, 'selectDaysForSeries', choices = vaccDays$days, selected = character(0))
    updatePickerInput(session, 'selectGenesProbesForSeries', choices = character(0), selected = character(0))
    
    updateSelectInput(session, 'selectColumnForModuleSeriesVaccines', choices = vaccDays$vaccines, selected = character(0))
    updateSelectInput(session, 'selectColumnForModuleSeriesDays', choices = vaccDays$days, selected = character(0))
    
    updatePickerInput(session, 'pickerGeneProbeLookup', choices = colnames(allData$data)[grepl("_",colnames(allData$data)) == FALSE], selected = 'Gene')
    updatePickerInput(session, 'selectKeywordColumn', choices = colnames(allData$data)[grepl("_",colnames(allData$data)) == FALSE], selected = 'Gene')
    
    updateTextInput(session, 'muscle_textInputKeyword', value = character(0))
    
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
  # dont reset things if we are reloading a saved search
  if(is.null(get("selectedVaccineDayFromSavedSearchGene", envir = .GlobalEnv))) {
    updateExpressionValueRangeVaccDay(sortCol_Probes)
    if(picker == 'vacc') {
      resetShapeNumericsToVaccine()
    }
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
    updatePickerInput(session, 'selectColumnDay', 
                      choices = dayPatterns[[input$selectColumnVaccine]], 
                      # we may be trying to set a specific day from a saved search so check if one is available
                      selected = get("selectedVaccineDayFromSavedSearchGene", envir = .GlobalEnv)
                      )
    updatePickerInput(session, 'selectShapeDay', choices = dayPatterns[[input$selectColumnVaccine]])
    respondToChangeColumn("vacc")
    # reset the selectedVaccineDayFromSavedSearchGene as we dont want it persisting
    assign("selectedVaccineDayFromSavedSearchGene",NULL, envir = .GlobalEnv)
    
  })


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
      if(input$checkboxGeneSearchWholeWord == FALSE) {
        showNotification("When searching columns that are not free text, it is better to keep Whole Word selected otherwise you will match partial names - which may be not what you intend.", type = 'warning')
      }
    }, ignoreInit = TRUE)
  
  
############### Apply Selection ### ####
  topGenesFoundString <- reactiveVal(NULL)
  output$textFiltersProbesFound <- renderText({topGenesFoundString()})
  topGenesAndModules <- reactiveVal()
  topGenesAndModules(NULL)
  
  source("searches.R")
  makeSearchGenes <- function(){
    return(
      list(
        savedSearchType = "gene",
        timestamp = format(Sys.time()),
        name = "Saved Gene Search",
        inputVals = map(geneinputs,~input[[.]]),
        inputFuns = geneinputFuns,
        kinetics = list(FileName = allData$folder, KinsType = "actual", Kinetics = shapeKinetics()),
        selectColumnDay = input$selectColumnDay
      )
    )
  }
  
loadSavedSearchGenes <- function(savedsearch){
  if (!is.null(savedsearch) && ("savedSearchType" %in% names(savedsearch)) && savedsearch$savedSearchType == "gene") {
    {
      assign("selectedVaccineDayFromSavedSearchGene",savedsearch$selectColumnDay, envir = .GlobalEnv)
      walk(names(savedsearch$inputVals),
       function(inpt){
         do.call(
           what = savedsearch$inputFuns[[inpt]],
           args = set_names(c(session,inpt,savedsearch$inputVals[[inpt]]),
                            c("session","inputId",argNameForInputFun(savedsearch$inputFuns[[inpt]])))
         )
       })
      makeNewKineticsFromFile(savedsearch$kinetics)
    }
  } else showNotification("There was a problem loading the saved search", type = 'error')
}

observeEvent(input$buttonResetFiltersGene,
  {
    loadSavedSearchGenes(read_rds("blankgene.rds"))
  })

observeEvent(input$buttonLoadSavedSearchGenes,
 {if (!is.null(input$buttonLoadSavedSearchGenes)) loadSavedSearchGenes(read_rds(input$buttonLoadSavedSearchGenes$datapath))
 })
  
output$buttonSaveSearchGenes <- downloadHandler(
  filename = function(){
    paste0(if_else(nchar(input$textSavedSearchGeneName)==0,"Gene Filters",input$textSavedSearchGeneName),".rds")},
  content = function(file) {write_rds(makeSearchGenes(),file)})
  

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
              
              # take out lincRNA from the start
              if(input$checkboxExcludeLINC == TRUE) {
                geneslist <- filter(geneslist, lincRNA == FALSE)
                filterSubText <-  paste0(filterSubText,'-lincRNA • ')
              }
              
              # apply the filters sequentially, do regex first before gene averages in getSortedGenesForVaccDay strips description
              if(input$checkboxSelectKeyword == TRUE) {
                # ignore an empty search
                if(input$textInputKeyword == "") {
                  showNotification("Empty keyword searches are ignored. No keyword filter has been applied.", type = 'error')
                } else {
                  if(input$checkboxGeneSearchWholeWord == FALSE) {
                    showNotification("Warning: when searching columns other than 'Description' without having Whole Word selected you will match partial names - which may be not what you intend.", type = 'warning')
                  }
                  geneslist <- getGenesForSearch(
                    geneslist,
                    input$textInputKeyword,
                    input$selectKeywordColumn,
                    input$checkboxGeneSearchWholeWord,
                    TRUE, # we always trim spaces input$checkboxGeneSearchStripSpaces,
                    # we use filter so if the match is TRUE it is included
                    input$radioKeywordIncludeExclude == "in"
                  )
                  if(dataFrameOK(geneslist)) {
                    
                    filterSubText <-  paste0(filterSubText," ",
                                             input$radioKeywordIncludeExclude,"clude ",'"',
                                             if_else(nchar(input$textInputKeyword)>40,
                                                     str_trunc(input$textInputKeyword,width = 30,side = "right"),input$textInputKeyword),
                                             '" • ')}
                }
              } 

              # kinetics also acts on allData$data, working on Probes so continue with that to reduce
              assign("selectKinetics",input$checkboxSelectValues == TRUE && input$radioFilterByRowKinetics == 'kinetics', envir = .GlobalEnv)
              if(selectKinetics == TRUE && dataFrameOK(geneslist)) {
                geneslist <- getGenesForKinetics(geneslist,shapeKinetics(), input$selectColumnVaccine,input$checkboxProbesGenes)
                if(dataFrameOK(geneslist)) {
                  filterSubText <-  paste0(filterSubText," match ",kineticsString(shapeKinetics())," • ")
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
                    paste0(gsub('_',' ',sortCol_Probes),' • ',filterSubText,' • ',
                           ifelse(input$checkboxDescending == TRUE, ' Sort ↓ ',' Sort ↑ '), ifelse(input$checkboxRowsAnyDay == TRUE," All Days • ",' • '),
                           ifelse(input$checkboxProbesGenes == TRUE, ' Gene Means',' Probes')
                    ))
                  dataAndFiltersText(paste0(allData$folder,': ',filtersText()))
                } else {
                  filtersText(paste0(gsub('_',' ',sortCol_Probes),' • [No filters] • ',
                                     ifelse(input$checkboxDescending == TRUE, ' Sort Descending • ',' Sort Ascending • '),
                                     ifelse(input$checkboxProbesGenes == TRUE, ' Gene Means',' Probes')))
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
                  foundStr <- paste0("Found: ",nrow(topGenesAndModules()[['genes']]),
                                     ifelse(input$checkboxProbesGenes == TRUE, ' genes', ' probes'), " and ",
                                     nModules, " modules")
                  topGenesFoundString(foundStr)
                  showNotification(paste0(foundStr," using filter: ", filtersText()), type = 'message')
              } else {
                topGenesAndModules(list(genes = NULL, modules = NULL, modsOnly = NULL))
                topGenesFoundString(NULL)
                removeNotification(id = "buttonApplySelection")
                showNotification(paste0("Found 0 Probes and 0 Modules"," using filter: ", filtersText()), type = 'warning')
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
      updateplotModuleSeriesBRUSH(list(Table = NULL, ProbeName = NULL, Gene = NULL))
      
      updateSelectInput(session, 'selectColumnForModuleSeriesVaccines')
      updateSelectInput(session, 'selectColumnForModuleSeriesDays')
      
      output$plotTopGenesSeries <- renderPlot({NULL})
      output$datatableTopGenesSeries <- renderDataTable({NULL})
      updateplotTopGenesSeriesBRUSH(list(Table = NULL, ProbeName = NULL, Gene = NULL))
      
      updateSelectInput(session, 'selectVaccinesForSeries')
      updateSelectInput(session, 'selectDaysForSeries')
      # need to determine if Probes or genes
      pgColname <- ifelse('ProbeName' %in% names(topGenesAndModules()[['genes']]) == FALSE,"Gene","ProbeName")
      assign("genesOrProbes",pgColname, envir = .GlobalEnv)
      updateSelectInput(session, 'selectGenesProbesForSeries', label = pgColname, choices = topGenesAndModules()[['genes']][[pgColname]], selected = topGenesAndModules()[['genes']][[pgColname]])
      
      updateTextInput(session, 'muscle_textInputKeyword', value = paste(topGenesAndModules()[['genes']][["Gene"]], collapse = ","))
      output$muscleplotIndividualsFilteredSortedProbesIndividuals <- renderPlot({NULL})
      # assign("muscleGGplotIndividuals",NULL, envir = .GlobalEnv)
      filteredSortedProbesTidyMuscle(NULL)
      tissueVaccineHourFilteredMuscle(NULL)
      
      
      if(genesOrProbes == "Gene") {show(id = "checkboxShowProbesOfGenesSeries")} 
      else {hide(id = "checkboxShowProbesOfGenesSeries")}
    }
  )
  
  
  #################### Top Probes #########################
  # output top genes
  output$buttonSaveTableTopGenesUpPlot <- downloadHandler(filename = function(){paste0("Selected Probes-Genes.png")},
    content = function(file) {plotDataTable(topGenesAndModules()[['genes']],file,35,input$checkboxHiRes_topgenes)})
  
  dataFilterStr <- function(t) {
    switch (t,
      'g' = return(paste0(allData$folder,'\n# ',filtersText())),
      'm' = return(paste0(allData$folder,'\n# ',modulesAndFiltersText()))
    )
  }
  
  addDescriptionToGenes <- function(genes){
    annot <- select(allData$data,-contains("_"))
    genes %>%
      mutate(
        Description = map_chr(Gene,function(g){
          paste(unique(lookupGenesProbes(g,annot, "Gene", TRUE,TRUE)[["Description"]]),
                collapse = " • ")
        })
      )
  }
  
  output$datatableTopGenesUp <- renderDataTable({
    addDescriptionToGenes(topGenesAndModules()[['genes']])
    },options = list(pageLength = 10))
  output$buttonSaveTableProbes <- downloadHandler(filename = function(){paste0("Selected Probes-Genes.csv")},
    content = function(file) {write_excel_csv(
      addDescriptionToGenes(topGenesAndModules()[['genes']]),
      file, na = "")})
  
  output$buttonSaveListGenes <- downloadHandler(filename = function(){
    return("Selected Probes Genes.txt")
    },
   content = function(file) {write_lines(
     makeTextListOfFilteredGenes(topGenesAndModules()[['genes']],
                                 allData$data,
                                 colnames(topGenesAndModules()[['genes']]),
                                 dataAndFiltersText()),
     file)})
  
  #################### Top Probes Series #########################
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
      
      output$datatableTopGenesSeries <- renderDataTable({topGenesInSeries},options = list(pageLength = 10))
      updateplotTopGenesSeriesBRUSH(list(Table = NULL, ProbeName = NULL, Gene = NULL))
      
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
        content = function(file) {printPlotPNG(ggplotTopGenesInSeries,file,session$clientData[["output_plotTopGenesSeries_height"]],
                                               session$clientData[["output_plotTopGenesSeries_width"]],
                                               input$checkboxHiRes_topgenesseries)})
      
    })
  
  updateplotTopGenesSeriesBRUSH <- function(res) {
    output$plotTopGenesSeriesBRUSH <- renderTable({res$Table}, striped = TRUE)
    output$plotTopGenesSeriesGENEMOD <- renderText({res$GeneMod})
    output$plotTopGenesSeriesSPOT <- renderText({res$ProbeName})
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
  
  output$buttonSaveTableProbesSeries <- downloadHandler(filename = function(){paste0("Selected Probes-Genes Series.csv")},
     content = function(file) {write_excel_csv(topGenesInSeries, file, na = "")})
  

  #################### Genes->Modules #########################
  # output assoc modules
  output$datatableGenesModules <- renderDataTable({
    topGenesAndModules()[['modules']]
  },options = list(pageLength = 10))

  output$buttonSaveTableGenesModules <- downloadHandler(filename = function(){paste0("Selected Genes -> Modules.csv")},
     content = function(file) {write_excel_csv(topGenesAndModules()[['modules']], file, na = "")})
  output$buttonSaveTableGenesModulesPlot <- downloadHandler(filename = function(){paste0("Selected Genes -> Modules.png")},
    content = function(file) {plotDataTable(topGenesAndModules()[['modules']],file,35,input$checkboxHiRes_tablemodules)})
  
  
  #################### Modules #########################
  # get the individual gene values for boxplot and the summ stats of the modules
  geneExpressionsForModules <- reactive({
    getExpressionsForModules(topGenesAndModules(),sortCol_Probes,allData$data, input$checkboxShowPsuedoModuleGenesModules,filtersText())})
  # draw / save table
  output$datatableSelModulesOnly <- renderDataTable({geneExpressionsForModules()[['Module']]},options = list(pageLength = 10))
  output$buttonSaveTableModulesSummary <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Summary.csv")},
    content = function(file) {write_excel_csv(geneExpressionsForModules()[['Module']], file, na = "")})
  output$buttonSaveTableModulesRaw <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Raw.csv")},
    content = function(file) {write_excel_csv(geneExpressionsForModules()[['expressions']], file, na = "")})
  
  output$buttonSaveTableModulesSummaryPlot <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Table.png")},
    content = function(file) {plotDataTable(geneExpressionsForModules()[['Module']],file,10.9,input$checkboxHiRes_modulessummary)})
  output$buttonSaveTableModulesSummaryListPlot <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes-Table.png")},
    content = function(file) {plotDataTable(select(geneExpressionsForModules()[['Module']],Module),file,20,input$checkboxHiRes_modulessummary)})
  
  output$buttonSaveTableModulesSummaryList <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes.txt")},
    content = function(file) {write_lines(
      paste0(
      paste(unique(filter(geneExpressionsForModules()[['Module']], Module != "Selected")[['Module']]), collapse = ','),'\n\n',
      paste(unique(filter(geneExpressionsForModules()[['Title']], Title != "Selected")[['Title']]), collapse = ','),'\n\n# ',
      dataFilterStr('g')
      ), file)})
  
  
  # draw / save plot
  ggplotGenesModules <-
    reactive({plotGenesModules(geneExpressionsForModules(),dataAndFiltersText(),
                input$checkboxShowLegendGenesModules, input$checkboxShowZeroGenesModules,input$checkboxGGplotGenesModules,
                input$radioGroupProbeModulesBy,
                if(input$checkboxModMedianMin == TRUE) input$numericModulesMedianMin else NULL,
                if(input$checkboxModMedianMax == TRUE) input$numericModulesMedianMax else NULL
                )})
  output$plotGenesModules <- renderPlot({ggplotGenesModules()} ,res = 72)
  output$plotGenesModulesSIZE <- renderUI({plotOutput("plotGenesModules", height = input$numberPlotGenesModulesSIZEheight)})
  output$buttonPNGplotGenesModules <- downloadHandler(filename = function(){paste0("Modules Of Selected Genes.png")},
    content = function(file) {printPlotPNG(ggplotGenesModules(),file,session$clientData[["output_plotGenesModules_height"]],
                                           session$clientData[["output_plotGenesModules_width"]],
                                           input$checkboxHiRes_genesmodules)})
  
  
  #################### Modules->Genes #########################
  # link the module select to the modules for top genes topGenesAndModules()[['modsOnly']]
  mods4Genes <- reactive({moduleDescriptionsForGenes(geneExpressionsForModules()[['Module']])})
  # change choices in the Genes In Module select based on selected modules
  observeEvent(mods4Genes(),{
    updatePickerInput(session, 'selectModuleForGenes', choices = mods4Genes())
    updateSelectInput(session, 'selectModuleForSeries', choices = mods4Genes())})
  # calculate gene expressions for the module selected
  expressionsInModule <- reactive({getGeneExpressionsInModule(input$selectModuleForGenes,sortCol_Probes,
                                                              allData$data,topGenesAndModules()[['genes']])})
  # redraw the table of gene expressions for the module selected
  output$datatableModuleGenes <- renderDataTable({expressionsInModule()},options = list(pageLength = 10))
  output$buttonTableModulesGenesList <- downloadHandler(filename = function(){paste0("Genes In ",input$selectModuleForGenes,".txt")},
    content = function(file) {write_lines(paste0(paste(rev(unique(expressionsInModule()[['Gene']])), collapse = ','),'\n\n# ',
      input$selectModuleForGenes,'\n\n# ',
      dataFilterStr('g')), file)})
  
  
  
  output$buttonSaveTableModulesGenes <- downloadHandler(filename = function(){paste0("Selected Genes-",input$selectModuleForGenes,"-Genes.csv")},
     content = function(file) {write_excel_csv(expressionsInModule(), file, na = "")})
  
  ggplotModuleGenes <- reactive({plotModuleGenes(expressionsInModule(),isolate(input$selectModuleForGenes),
                                dataAndFiltersText(),input$checkboxShowLegendModuleGenes, input$checkboxShowZeroModuleGenes,
                                input$checkboxGGplotModuleGenes, input$checkboxShowMissingModuleGenes)})
  output$plotModuleGenes <- renderPlot({ggplotModuleGenes()} ,res = 72)
  output$plotModuleGenesSIZE <- renderUI({plotOutput("plotModuleGenes", height = input$numberPlotModuleGenesSIZEheight)})
  output$buttonPNGplotModuleGenes <- downloadHandler(filename = function(){paste0("Selected Genes-",input$selectModuleForGenes,".png")},
     content = function(file) {printPlotPNG(ggplotModuleGenes(),file,
                                            session$clientData[["output_plotModuleGenes_height"]],
                                            session$clientData[["output_plotModuleGenes_width"]],
                                            input$checkboxHiRes_plotmodulesgenes)})
  
  #################### Modules Series #########################
  # selectModuleForSeries and selectColumnForModuleSeries are updated above
  assign("moduleValues",NULL, envir = .GlobalEnv)
  assign("ggplotModulesInSeries",NULL, envir = .GlobalEnv)
  observeEvent({
    input$buttonPlotModuleSeries
  },{
    output$plotModuleSeries <- renderPlot({NULL})
    updateplotModuleSeriesBRUSH(list(Table = NULL, ProbeName = NULL, Gene = NULL))
    
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
    
    output$datatableModuleSeries <- renderDataTable({moduleValues},options = list(pageLength = 10))

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
    content = function(file) {printPlotPNG(ggplotModulesInSeries,file,session$clientData[["output_plotModuleSeries_height"]],
                                           session$clientData[["output_plotModuleSeries_width"]],
                                           input$checkboxHiRes_modulesseries)})
  
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
    {if(input$radioRibbonBoxModuleSeries != 'Boxplot') updateplotModuleSeriesBRUSH(handleBrush(moduleValues,
        input$brush_plotModuleSeries,
        input$checkboxShowFacetModuleSeries,
        FALSE,"Value"))
  })
  
  
  output$buttonSaveTableModulesSeries <- downloadHandler(filename = function(){paste0("Selected Genes-Modules Series.csv")},
    content = function(file) {write_excel_csv(moduleValues, file, na = "")})
  
  observeEvent(input$buttonAddAllModulesModuleSeries,{updateSelectInput(session, 'selectModuleForSeries', selected = mods4Genes())})
  observeEvent(input$buttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'selectModuleForSeries', selected = character(0))})
  
  observeEvent(input$buttonAddAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'selectColumnForModuleSeriesVaccines', selected = vaccinesDaysFromColNames(allData$colNames)[['vaccines']])})
  observeEvent(input$buttonRemoveAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'selectColumnForModuleSeriesVaccines', selected = character(0))})
  observeEvent(input$buttonAddAllColumnsModuleSeriesDays,{updateSelectInput(session, 'selectColumnForModuleSeriesDays', selected = vaccinesDaysFromColNames(allData$colNames)[['days']])})
  observeEvent(input$buttonRemoveAllColumnsModuleSeriesDays,{updateSelectInput(session, 'selectColumnForModuleSeriesDays', selected = character(0))})
  
  
  
  #################### Muscle #########################
  observeEvent(
    input$muscle_buttonRemoveAllGenes,
    {
      updateTextInput(session, 'muscle_textInputKeyword', value = character(0))
      output$muscleplotIndividualsFilteredSortedProbesIndividuals <- renderPlot({NULL})
      # assign("muscleGGplotIndividuals",NULL, envir = .GlobalEnv)
      filteredSortedProbesTidyMuscle(NULL)
      tissueVaccineHourFilteredMuscle(NULL)
      
    }
  )
  
  observeEvent(input$muscle_buttonApplySelection,
               {
                 showNotification("Please wait while genes plotted…")
                 
                 selectedTissVaccHour <- paste0("CRC305E ",
                                                isolate(input$muscle_selectColumnVaccine)," ",
                                                isolate(input$muscle_selectColumnTissue),"_",
                                                isolate(input$muscle_selectColumnHour))
                 filteredSortedRows <- select(allData$muscleIndividuals,ProbeName,Gene,starts_with(paste0(selectedTissVaccHour,"•")))
                 # do keyword search
                 filteredSortedRows <- 
                   getGenesForSearch(filteredSortedRows,input$muscle_textInputKeyword,"Gene",input$muscle_checkboxGeneSearchWholeWord,TRUE,TRUE) %>%
                   gather(key = "Participant",value = "FC",-c(ProbeName,Gene)) %>%
                   mutate(Participant = as.factor(gsub(paste0(selectedTissVaccHour,"•CRC305E-"),"",Participant)))
                 
                 medianFilteredSortedRows <- filteredSortedRows %>%
                   group_by(Gene) %>%
                   summarise(medianFC = median(FC,na.rm = TRUE)) %>%
                   arrange(desc(medianFC))
                 
                 filteredSortedRows <- filteredSortedRows %>%
                   mutate(Gene = factor(Gene,levels = unique(medianFilteredSortedRows$Gene)))
                 
                 # set the global variables
                 filteredSortedProbesTidyMuscle(filteredSortedRows)
                 tissueVaccineHourFilteredMuscle(paste(isolate(input$muscle_selectColumnVaccine),
                                                       isolate(input$muscle_selectColumnTissue),
                                                       isolate(input$muscle_selectColumnHour)))
                 
                 if(!is.null(filteredSortedProbesTidyMuscle()) &&
                    nrow(filteredSortedProbesTidyMuscle())>0) {
                   muscleGGplotIndividuals <- 
                     ggplot(data =  filteredSortedProbesTidyMuscle(), mapping = aes_string(x = "Gene",y = "FC")) +
                     themeBaseMuscle(TRUE) +
                     geom_hline(yintercept = 0, linetype = 2, color = 'black', size = 0.8) +
                     geom_boxplot(mapping = aes(fill = Gene), alpha = 0.2, outlier.alpha = 0.0, show.legend = FALSE) + # dont show outlier dots as they are already plotted as geom_poins
                     geom_point(mapping = aes(color = Participant, shape = Participant), alpha = 0.9, size = 5, position = position_jitter(width = 0.15, height = 0)) +
                     scale_y_continuous(name = paste("Log2 Fold Change From",if_else(isolate(input$muscle_selectColumnTissue) == "Muscle","Unimmunised Leg","Pre Immunisation"))) +
                     scale_fill_viridis_d(option = "A", direction = -1) +
                     ggtitle(label = paste(tissueVaccineHourFilteredMuscle()," days - individual probes for genes"))
                   
                   # return(p)
                 }
                 else {
                   muscleGGplotIndividuals <- NULL
                   # return(NULL)
                 }
                 output$muscleplotIndividualsFilteredSortedProbesIndividuals <- renderPlot({muscleGGplotIndividuals})
                 output$buttonSavemuscleplotIndividualsFilteredSortedProbesIndividualsSIZE <- downloadHandler(
                   filename = function(){paste0(tissueVaccineHourFilteredMuscle(),".png")},
                   content = function(file) {
                      printPlotPNG(muscleGGplotIndividuals,
                      file,session$clientData[["output_muscleplotIndividualsFilteredSortedProbesIndividuals_height"]],
                      session$clientData[["output_muscleplotIndividualsFilteredSortedProbesIndividuals_width"]],
                      input$checkboxHiRes_muscleindividuals)}
                   )
               },
               ignoreInit = TRUE)
  
  output$muscleplotIndividualsFilteredSortedProbesIndividualsSIZE <- 
    renderUI({plotOutput("muscleplotIndividualsFilteredSortedProbesIndividuals", height = isolate(input$muscle_PlotGenesSIZEheight))})
  
  output$muscle_filteredSortedProbesTidyMuscle <- renderDataTable({filteredSortedProbesTidyMuscle()},options = list(pageLength = 10))
  
  
  output$buttonSaveTablemuscle_filteredSortedProbesTidyMuscle <- downloadHandler(
    filename = function(){paste0(tissueVaccineHourFilteredMuscle(),".csv")},
    content = function(file) {write_excel_csv(filteredSortedProbesTidyMuscle(), file, na = "")})
  #################### GO #########################
  assign("goPalette",NULL, envir = .GlobalEnv)
  selectedGOdata <- reactiveVal(NULL)
  selectedGOdataAllGenesForAnnos <- reactiveVal(NULL)
  selectedGOdataSummary <- reactiveVal(NULL)
  plotGOGEdata <- reactiveVal(NULL)
  plotGOGEdataGOtermMeans <- reactiveVal(NULL)
  plotGOplot <- reactiveVal(NULL)
  textGOmatched <- reactiveVal(NULL)
  textGONotMatched <- reactiveVal(NULL)
  
  observeEvent(
    {
      input$go_selectColumnVaccine
    },
    {
      if(is.null(dayPatterns[[input$selectColumnVaccine]])) {showNotification(type = 'error', ui = paste0("There is a problem with data formatting - no day numbers could be found for ",input$selectColumnVaccine))}
      updateSelectInput(session, 'go_selectColumnDay', choices = dayPatterns[[input$go_selectColumnVaccine]], selected = character(0))
    })
  
  observeEvent(
    input$go_buttonClear,
    {
      updateTextInput(session, 'go_textInputKeyword', value = character(0))
      selectedGOdata(NULL)
    },ignoreNULL = TRUE, ignoreInit = TRUE
  )
  
  observeEvent(input$go_buttonAddAllVaccDays,{updateSelectInput(session, 'go_selectColumnDay', selected = dayPatterns[[input$go_selectColumnVaccine]])})
  observeEvent(input$go_buttonRemoveAllVaccDays,{updateSelectInput(session, 'go_selectColumnDay', selected = character(0))})
  
  
  observeEvent(
    selectedGOdata(),
    {
      removeNotification("calcGOnotify")
    }, ignoreNULL = FALSE,ignoreInit = TRUE
  )
  observeEvent(
    plotGOplot(),
    {
      removeNotification("plotGOnotify")
    }, ignoreNULL = FALSE,ignoreInit = TRUE
  )
  
  observeEvent(input$go_buttonGO,
               {
                 if(!is.null(allData$goData)) {
                   selectedGOdata(NULL)
                   showNotification("Please wait while GO terms filtered…",id = "calcGOnotify",duration = 120)
                   search <- getSearchItemsFromCommaList(input$go_textInputKeyword,wholeWord = FALSE,stripSpaces = TRUE,makeUpper = TRUE)
                   godata <- allData$goData %>%
                     filter(Gene %in% search) %>%
                     select(Gene,everything()) %>%
                     arrange(Gene,GOdomain,GOaccession,GOterm)
                   selectedGOdata(godata)
                   
                   gotermsSelected <- unique(godata$GOterm)
                   assign("goPalette",purrr::set_names(sample(rainbow(length(gotermsSelected))),nm = gotermsSelected), envir = .GlobalEnv)
                   
                   # set what we did / not find
                   textGOmatched(paste(unique(godata$Gene),collapse = ","))
                   textGONotMatched(paste(setdiff(search,unique(godata$Gene)),collapse = ","))
                   
                   godataNgenes <- godata %>%
                     group_by(GOdomain,GOaccession,GOterm) %>%
                     summarise(
                       GeneCount = n(),
                       Genes = paste(Gene, collapse = ",")
                     ) %>%
                     arrange(GOdomain,desc(GeneCount))
                   selectedGOdataSummary(godataNgenes)
                   
                   
                   updateCheckboxGroupInput(session, 'go_select_MF',choices = filter(godataNgenes,GOdomain == "MF")[["GOterm"]],selected = character(0))
                   updateCheckboxGroupInput(session, 'go_select_BP',choices = filter(godataNgenes,GOdomain == "BP")[["GOterm"]],selected = character(0))
                   updateCheckboxGroupInput(session, 'go_select_CC',choices = filter(godataNgenes,GOdomain == "CC")[["GOterm"]],selected = character(0))
                   
                   
                   # create the df with all genes for all annos
                   selectedgodataAllGenesForAllAnnos <- allData$goData %>%
                     filter(GOterm %in% gotermsSelected) %>%
                     select(GOterm,everything()) %>%
                     arrange(GOdomain,GOaccession,GOterm,Gene)
                   selectedGOdataAllGenesForAnnos(selectedgodataAllGenesForAllAnnos)
                   
                   
                 } else {
                   selectedGOdata(NULL)
                   selectedGOdataSummary(NULL)
                   selectedGOdataAllGenesForAnnos(NULL)
                 }
               }, ignoreInit = TRUE
  )
  
  observeEvent(
    input$go_buttonPlot,
    {
      # c() ignores NULL
      goas2plot <- c(input$go_select_CC,input$go_select_MF,input$go_select_BP)
      if(length(goas2plot)>0 && !is.null(selectedGOdata()) && length(input$go_selectColumnDay)>0) {
        plotGOplot(NULL)
        showNotification("Please wait while GO terms are plotted …",id = "plotGOnotify",duration = 120)
        
        # put in the relevant dataset if allgenesallannos requested
        selectedgodata <- if(input$go_checkboxGOAllGenesAllAnnos == TRUE) selectedGOdataAllGenesForAnnos() else selectedGOdata()
        
        GEdataSelectedDays <- allData$data %>%
            select(Gene,paste(input$go_selectColumnVaccine,input$go_selectColumnDay,sep = "_"))
          
        goAllData <- 
            map_dfr(goas2plot, function(goterm){
              genes4goterm <- selectedgodata %>%
                filter(
                  GOterm == goterm
                )
      
              GEdataForDaysGOterm <- GEdataSelectedDays %>%
                filter(Gene %in% unique(genes4goterm$Gene)) %>%
                group_by(Gene) %>%
                # summarise mean but could split probes here
                summarise_if(is.numeric,mean,na.rm = TRUE)
              
              GEdataAndGOterms <- full_join(genes4goterm,GEdataForDaysGOterm, by = "Gene")
      
              GEdataAndGOterms <- GEdataAndGOterms %>%
                gather(key = "Day", value = "Expression", -c(Gene,GOterm,GOdomain,GOaccession), convert = FALSE,factor_key = FALSE) %>%
                mutate(
                  Day = as.numeric(gsub(paste0(input$go_selectColumnVaccine,"_"),"",Day)),
                  # make an unique gene-term so we can plot genes that map several terms separately
                  GeneGOaccession = paste(Gene, GOaccession)
                  )
          })
        plotGOGEdata(goAllData)
        
        goAllDataTerms <- goAllData %>%
          group_by(GOterm,Day) %>%
          summarise(
            Ymax = mean_se(Expression)$ymax,
            Ymin = mean_se(Expression)$ymin,
            SEM = Ymax-Ymin,
            Expression = mean_se(Expression)$y
          )
        
        plotGOGEdataGOtermMeans(goAllDataTerms)
        
        plotGOplot(ggplotGO(
          data2plot = if(input$go_checkboxGOtermMeans == TRUE) goAllDataTerms else goAllData,
          vacc = input$go_selectColumnVaccine,
          ismeans = input$go_checkboxGOtermMeans,
          jitterX = input$go_checkboxJitterX,
          Grouper = if_else(input$go_checkboxGOtermMeans, "GOterm", "GeneGOaccession"),
          pal = if(input$go_checkboxUsePalette) goPalette else NULL,
          sems = input$go_checkboxSEM,
          leg = input$go_checkboxUseLegend
        ))
    }
    },ignoreNULL = TRUE, ignoreInit = TRUE
  )
  
  output$textGOmatched <- renderText({textGOmatched()})
  output$textGONotMatched <- renderText({textGONotMatched()})
  
  # GOselectedAnnos <- reactiveVal(c(input$go_select_CC,input$go_select_MF,input$go_select_BP))
  output$textGOselectedAnnos <- renderText({paste(c(input$go_select_CC,input$go_select_MF,input$go_select_BP),collapse = " • ")})
  
  output$go_plotGenesSeriesSIZE <- renderUI({tagList(
    plotOutput("go_plotGenesSeries", 
               height = input$go_plotGenesSeriesSIZEheight,
               click = "click_go_plotGenesSeries",
               brush = "brush_go_plotGenesSeries"
    ))})
  
  output$go_plotGenesSeries <- renderPlot({ plotGOplot()})
  
  updateBrushClickGO <- function(res){
    output$go_plotGenesSeries_clicktable <- renderTable({
      if("GeneGOaccession" %in% colnames(res$Table)) select(res$Table,-GeneGOaccession)
      else res$Table
      }, striped = TRUE)
    output$go_plotGenesSeries_clickTextGene <- renderText({res$GeneMod})
    output$go_plotGenesSeries_clickTextGOterm <- renderText({res$Goterm})
  }
  
  observeEvent(
    input$click_go_plotGenesSeries, 
    {
      res <- handleClick(if(input$go_checkboxGOtermMeans == TRUE) plotGOGEdataGOtermMeans() else plotGOGEdata(),
                         input$click_go_plotGenesSeries,FALSE,FALSE,"Expression","Day","")
      if(!is.null(res$Table)) {
        updateBrushClickGO(res)
      }
    })
  
  observeEvent(input$brush_go_plotGenesSeries,
    {
      updateBrushClickGO(handleBrush(if(input$go_checkboxGOtermMeans == TRUE) plotGOGEdataGOtermMeans() else plotGOGEdata(),
                                                input$brush_go_plotGenesSeries,FALSE,FALSE,"Expression","Day",""))
    })
  
  output$go_plotGenesSeries_download <- 
    downloadHandler(filename = function(){paste0("GO kinetics.png")},
                    content = function(file) {
                      printPlotPNG(plotGOplot(),
                                   file,
                                   session$clientData[["output_go_plotGenesSeries_height"]],
                                   session$clientData[["output_go_plotGenesSeries_width"]],
                                   input$checkboxHiRes_go_plotGenesSeries)})
  
  
  
  output$go_datatablePlotDataGOtermMeans <- renderDataTable({
    if(is.null(plotGOGEdataGOtermMeans())) return(NULL)
    plotGOGEdataGOtermMeans()},options = list(pageLength = 10))
  output$go_datatablePlotDataGOtermMeans_download <- downloadHandler(
    filename = function(){paste0("GO kinetics plot data GOterm means.csv")},
    content = function(file) {write_excel_csv(plotGOGEdataGOtermMeans(), file, na = "")})

  
  output$go_datatablePlotData <- renderDataTable({
    if(is.null(plotGOGEdata())) return(NULL)
    select(plotGOGEdata(),-GeneGOaccession)},options = list(pageLength = 10))
  output$go_datatablePlotData_download <- downloadHandler(
    filename = function(){paste0("GO kinetics plot data.csv")},
    content = function(file) {write_excel_csv(plotGOGEdata(), file, na = "")})
  
  
  output$go_datatableGO <- renderDataTable({
    if(is.null(selectedGOdata())) return(NULL)
    selectedGOdata()},options = list(pageLength = 10))
  output$go_datatableGO_download <- downloadHandler(
    filename = function(){paste0("GOterms for genes data.csv")},
    content = function(file) {write_excel_csv(selectedGOdata(), file, na = "")})
  
  
  output$go_datatableGOSummary <- renderDataTable({
    if(is.null(selectedGOdataSummary())) return(NULL)
    selectedGOdataSummary()},options = list(pageLength = 10))
  output$go_datatableGOSummary_download <- downloadHandler(
    filename = function(){paste0("GOterms summary data.csv")},
    content = function(file) {write_excel_csv(selectedGOdataSummary(), file, na = "")})
  
  output$go_datatableGOAllAnnosGenes <- renderDataTable({
    if(is.null(selectedGOdataAllGenesForAnnos())) return(NULL)
    selectedGOdataAllGenesForAnnos()},options = list(pageLength = 10))
  output$go_datatableGOAllAnnosGenes_download <- downloadHandler(
    filename = function(){paste0("GOterms with all genes data.csv")},
    content = function(file) {write_excel_csv(selectedGOdataAllGenesForAnnos(), file, na = "")})
  
  ############################## Gene Lookup ###########
  assign("lookedupGenes",NULL, envir = .GlobalEnv)
  observeEvent({
    input$buttonGeneLookup
  },{
    assign("lookedupGenes",lookupGenesProbes(input$textInputGeneLookup, 
                                             select(allData$data,-contains("_")) ,
                                             input$pickerGeneProbeLookup,
                                             input$checkboxGeneLookupWholeWord,
                                             TRUE # we always trim spaces input$checkboxGeneLookupStripSpaces
                                             ),
           envir = .GlobalEnv)
    output$datatableGeneLookup <- renderDataTable({lookedupGenes},options = list(pageLength = 10))
  })
  observeEvent({
    input$buttonGeneLookupNone
  },{
    updateTextInput(session, 'textInputGeneLookup', value = "")
    output$datatableGeneLookup <- renderDataTable({NULL})
  })

  output$buttonSaveTableGeneLookup <- downloadHandler(filename = function(){paste0("Gene Lookup.csv")},
   content = function(file) {write_excel_csv(lookedupGenes, file, na = "")})
  
  
  ############################## Network ###########
  assign("vaccDaysToNet",tibble(srch = character(),dsply= character()), envir = .GlobalEnv)

  
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
      input$buttonAddVacDayNet
    },
    {
      vacDy <- paste0(input$selectVaccNet,"_",input$selectDayNet)
      if(input$radioGenesRankNet == "Genes") {
        newsrch <- tibble(
          srch = paste0(vacDy,"†",input$textSpecifiedGenesNet),
          dsply = paste(gsub("_", " ",vacDy),str_trunc(input$textSpecifiedGenesNet,15,"right"),
                        # need an unique tag as truncated genes may not be unique
                        " «",input$buttonAddVacDayNet,"»")
        )
      } else {
        s <- paste0(if_else(input$checkboxDescNet == TRUE,"↓","↑"),input$numericNumRowsNet)
        newsrch <- tibble(
          srch = paste0(vacDy,"†",s),
          dsply = paste0(gsub("_", " ",vacDy)," ",s) # dont need an unique tag as they are always unique
        )
      }
      df <- get("vaccDaysToNet", envir = .GlobalEnv) %>%
        bind_rows(newsrch) %>%
        distinct(srch, .keep_all = TRUE)
      assign("vaccDaysToNet",df, envir = .GlobalEnv)
      updatePickerInput(session, 'selectVacDaysToNet', choices = df[['dsply']], selected = df[['dsply']])
    })
  
  observeEvent(
    {
      input$buttonClearNet
    },
    {
      assign("vaccDaysToNet",tibble(srch = character(),dsply= character()), envir = .GlobalEnv)
      updatePickerInput(session, 'selectVacDaysToNet', choices = character(0), selected = character(0))
    })
  
  observeEvent(
    {
      input$selectVacDaysToNet
    },
    {
      if(is.null(input$selectVacDaysToNet)) {
        updatePickerInput(session, 'selectVacDaysToNet', choices = character(0), selected = character(0))
      } else {
        # take out the missing row
        assign("vaccDaysToNet",
               filter(get("vaccDaysToNet", envir = .GlobalEnv), dsply %in% input$selectVacDaysToNet), 
               envir = .GlobalEnv)
        updatePickerInput(session, 'selectVacDaysToNet', choices = input$selectVacDaysToNet, selected = input$selectVacDaysToNet)
      }
    }, ignoreNULL = FALSE)
  
  numRowsNet <- reactiveVal(NULL)
  networkEdgeListAndCount <- reactiveValues(edgeCount = NULL, edgeList = NULL)
  
  plotNetworkGraph <- function(){
    networkEdgeListAndCount$edgeCount <- NULL
    networkEdgeListAndCount$edgeList <- NULL
    networkEListAndCount <- getNetworkEdgeListAndCount(allData$data,get("vaccDaysToNet", envir = .GlobalEnv))
    # networkEListAndCount <- getNetworkEdgeListAndCount(allData$data,input$selectVacDaysToNet,input$numericNumRowsNet,input$checkboxDescNet)
    networkEdgeListAndCount$edgeCount <- networkEListAndCount[['edgeCount']]
    networkEdgeListAndCount$edgeList <- networkEListAndCount[['edgeList']]
    numRowsNet(paste(input$numericNumRowsNet,"rows", ifelse(input$checkboxDescNet == TRUE, "descending ", "ascending ")))
    updateNumericEdgeThresh()
  }
  
  observeEvent(
    {
      input$buttonPlotNet
    },
    {
      plotNetworkGraph()
    })
  
  netFilterString <- reactive({
    paste(numRowsNet(),
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
  },options = list(pageLength = 10))
  
  output$datatableEdgeCountNet <- renderDataTable({
    if(is.null(networkFilteredEdgeListAndCount()[['edgeCount']])) NULL
    else arrange(networkFilteredEdgeListAndCount()[['edgeCount']],desc(Connections))
  },options = list(pageLength = 10))
  
  vennVaccGenesList <- reactive({getVennVaccGenesList(networkFilteredEdgeListAndCount()[['edgeList']], isolate(input$selectVacDaysToNet))})
  
  output$datatableIntersectsNet <- renderDataTable({
    geneIntersectsFromVaccGenesList(vennVaccGenesList())
  },options = list(pageLength = 10))
  
  filenameForNet <- function(netType,fileType,vaccdays){
    paste0(
      trimws(paste0(
      switch(netType,"n" = "Network ","e" = "Euler ","v" = "Venn ","u" = "UpSetR ", 'x' = ""),
      vaccdays," ",
      netFilterString()
      )),
      fileType)
  }
  
  output$buttonSaveTablesNet <- downloadHandler(
    filename = function(){filenameForNet('x',".xlsx",input$selectVacDaysToNet)},
    content = function(file) {write.xlsx(
      list(
        Genes = if(is.null(networkFilteredEdgeListAndCount()[['edgeList']])) data.frame(Empty = "") else select(networkFilteredEdgeListAndCount()[['edgeList']],-c(revrank,MeanValueRound)),
        Connections = if(is.null(networkFilteredEdgeListAndCount()[['edgeCount']])) data.frame(Empty = "") else arrange(networkFilteredEdgeListAndCount()[['edgeCount']],desc(Connections)),
        Intersects = if(is.null(geneIntersectsFromVaccGenesList(vennVaccGenesList()))) data.frame(Empty = "") else geneIntersectsFromVaccGenesList(vennVaccGenesList())
        ),
      file)}
    )
  
  
  
  output$plotVenn <- renderPlot({
    if(is.null(vennVaccGenesList())) NULL
    else grid.draw(venDiagramFromVaccGenesList(vennVaccGenesList()))
  })
  output$plotEuler <- renderPlot({
    eulerFromVaccGenesList(vennVaccGenesList(),input$radioEulerShape)
  })
  output$plotUpset <- renderPlot({
    upsetrFromVaccGenesList(vennVaccGenesList(),input$radioUpsetOrder,input$checkboxEmptyintersections)
  })
  
  #input$plotNetSIZEheight below is to just react
  triggerQgraph <- reactiveValues(v = 0)
  # make it dependent on size values so it redraws
  networkQgraph <- reactive({
    triggerQgraph$v
    return(getNetworkQgraph(networkFilteredEdgeListAndCount(),input$radioNetType, 
      input$radioLineLabelVariableNet, input$checkboxLineLabelsNet,input$nodeAlphaNet,input$checkboxLegendNet))
    })
  output$plotNet <- renderPlot({
    if(!is.null(networkQgraph())) {
      plot(networkQgraph()[['qgraph']])
    }
    else NULL
  })
  output$plotNetworkLegend <- renderPlot({
    if(input$checkboxLegendNet && !is.null(networkQgraph())) plot(networkQgraph()[['plt']])
    else NULL
  })
  
  observeEvent(
    {
      input$windowResizeNet #<< this is a pseudo-input, a variable we created in the javascript inside the GUI
      input$plotNetSIZEheight
    },
    {
      # made networkQgraph() dependent on triggerQgraph$v a reactive value. If we change this we invalidate networkQgraph() and it redraws
      # we do this as the circles get mishapen when resized and have to be redrawn. 
      # the 0.5s delay ensures it completes all other drawing so we then update
      if(!is.null(networkQgraph())) delay(500,{triggerQgraph$v <- triggerQgraph$v+1})
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  output$plotNetSIZE <- renderUI({plotOutput("plotNet", height = input$plotNetSIZEheight)})
  output$plotVennSIZE <- renderUI({plotOutput("plotVenn", height = input$plotNetSIZEheight)})
  output$plotEulerSIZE <- renderUI({plotOutput("plotEuler", height = input$plotNetSIZEheight)})
  output$plotUpsetSIZE <- renderUI({plotOutput("plotUpset", height = input$plotNetSIZEheight)})
  
  output$buttonPNGNet <- downloadHandler(
    filename = function(){filenameForNet(input$radioVennNetworkeNet,".png",input$selectVacDaysToNet)},
    content = function(file) {
      # upsetR wants to draw directly and returns nothing
      if(input$radioVennNetworkeNet == "u") {
        upSetRPNG(vennVaccGenesList(),input$radioUpsetOrder,input$checkboxEmptyintersections,file,
                  session$clientData[["output_plotNet_height"]],session$clientData[["output_plotNet_width"]],
                  input$checkboxHiRes_network)
      }
      else plotPlotPNG(
      switch(input$radioVennNetworkeNet,
             "n" = networkQgraph()[['qgraph']],
             "e" = eulerFromVaccGenesList(vennVaccGenesList(),input$radioEulerShape),
             "v" = venDiagramFromVaccGenesList(vennVaccGenesList())
             ),
      file,session$clientData[["output_plotNet_height"]],session$clientData[["output_plotNet_width"]],
      input$radioVennNetworkeNet,input$checkboxHiRes_network)
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
  
  topModsFoundString <- reactiveVal(NULL)
  output$textFiltersModsFound <- renderText({topModsFoundString()})
  
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
            foundModsStr <- paste0("Found: ",length(unique(mods[["Module"]])), " modules")
            topModsFoundString(foundModsStr)
            showNotification(foundModsStr, type = 'message')
            
          } else {
            modulesAndFiltersText("")
            topModsFoundString(NULL)
            topModulesSelected(NULL)
            hide(id = "navModuleHeader")
            removeNotification(id = "mbuttonApplySelection")
            showNotification(paste0("No modules found"), type = 'warning')
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
    updatemplotModuleSeriesBRUSH(list(Table = NULL, ProbeName = NULL, Gene = NULL))
    
    updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', selected = character(0))
    updateSelectInput(session, 'mselectColumnForModuleSeriesDays', selected = character(0))
    updateSelectInput(session, 'mselectPlotModulesInSeries',choices = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))
    updatePickerInput(session, 'mselectModuleForGenes',choices = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))
    
  }
)

# output top genes
output$mdatatableTopModulesUp <- renderDataTable({topModulesSelected()},options = list(pageLength = 10))
output$buttonSaveTableTopModulesUpPlot <- downloadHandler(filename = function(){paste0("Selected By Modules.png")},
  content = function(file) {plotDataTable(topModulesSelected(),file,10.9,input$checkboxHiRes_mmodulesplot)})
output$buttonSaveTableTopModulesUOnlypPlot <- downloadHandler(filename = function(){paste0("Selected By Modules.png")},
  content = function(file) {plotDataTable(select(topModulesSelected(),Rank:Category),file,10.9,input$checkboxHiRes_mmodulesplot)})

output$mbuttonSaveTableModules <- downloadHandler(filename = function(){paste0("Selected By Modules.csv")},
  content = function(file) {write_excel_csv(topModulesSelected(), file, na = "")})

## amendThisNow
output$mbuttonSaveListTopModules <- downloadHandler(filename = function(){paste0("Selected By Modules.txt")},
  content = function(file) {
    write_lines(
      makeTextListOfFilteredModules(topModulesSelected(),
                                  dataFilterStr('m')),file)
    })

  # Plot Modules Selected #
ggplotSelectedModules <-
  reactive({
    plotSelectedModules(allData$modules,topModulesSelected(),modulesAndFiltersText(),
                        input$mcheckboxShowLegendGenesModules, input$mcheckboxShowZeroGenesModules,
                        input$mcheckboxModuleMedians,input$mradioGroupTitleName,
                        input$mcheckboxGGplotGenesModules,
                        if(input$mcheckboxModMedianMin == TRUE) input$mnumericModulesMedianMin else NULL,
                        if(input$mcheckboxModMedianMax == TRUE) input$mnumericModulesMedianMax else NULL
                        )})
output$mplotSelectedModules <- renderPlot({ggplotSelectedModules()} ,res = 72)
output$mplotSelectedModulesSIZE <- renderUI({plotOutput("mplotSelectedModules", height = input$numbermplotSelectedModulesSIZEheight)})
output$buttonPNGmplotSelectedModules <- downloadHandler(filename = function(){paste0("Selected Modules.png")},
  content = function(file) {printPlotPNG(ggplotSelectedModules(),file,
                                         session$clientData[["output_mplotSelectedModules_height"]],
                                         session$clientData[["output_mplotSelectedModules_width"]],
                                         input$checkboxHiRes_mplotmodules)})

#################### Modules Modules->Genes #########################

# change choices in the Genes In Module select based on selected modules
# observeEvent(topModulesSelected() is above and resets our menu
# calculate gene expressions for the module selected
expressionsInModuleModule <- reactive({getGeneExpressionsInModule(input$mselectModuleForGenes,sortCol_Mods,allData$data,NULL)})
# # redraw the table of gene expressions for the module selected
output$mdatatableModuleGenes <- renderDataTable({select(expressionsInModuleModule(),-Selected)},options = list(pageLength = 10))
output$buttonSaveTableModulesGenes <- downloadHandler(filename = function(){paste0("Genes in ",input$mselectModuleForGenes,".csv")},
                                                      content = function(file) {write_excel_csv(expressionsInModuleModule(), file, na = "")})

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
 content = function(file) {printPlotPNG(ggplotModuleModuleGenes(),file,
                                        session$clientData[["output_mplotModuleGenes_height"]],
                                        session$clientData[["output_mplotModuleGenes_width"]],
                                        input$checkboxHiRes_mplotmodulegenes)})

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
    output$mdatatableModuleSeries <- renderDataTable({ggplotSelectedModulesSeries[['table']]},options = list(pageLength = 10))
    updatemplotModuleSeriesBRUSH(list(Table = NULL, ProbeName = NULL, Gene = NULL))
    
})
output$buttonPNGmplotModuleSeries <- downloadHandler(filename = function(){paste0("Selected Modules Series.png")},
  content = function(file) {printPlotPNG(ggplotSelectedModulesSeries[['plot']],
                                         file,session$clientData[["output_mplotModuleSeries_height"]],
                                         session$clientData[["output_mplotModuleSeries_width"]],
                                         input$checkboxHiRes_mplotmoduleseries)})

updatemplotModuleSeriesBRUSH <- function(res) {
  output$mplotModuleSeriesBRUSH <- renderTable({res$Table}, striped = TRUE)
  output$mplotModuleSeriesGENEMOD <- renderText({res$GeneMod})
}

observeEvent(input$click_mplotModuleSeries, 
             {
               if(input$mradioRibbonBoxModuleSeries != 'Boxplot') {
               res <- handleClick(ggplotSelectedModulesSeries[['table']],
                                  input$click_mplotModuleSeries,
                                  input$mcheckboxShowFacetModuleSeries,
                                  FALSE,"Mean")
               if(!is.null(res$Table)) updatemplotModuleSeriesBRUSH(res)
               }
})

observeEvent(input$brush_mplotModuleSeries, 
             {
               if(input$mradioRibbonBoxModuleSeries != 'Boxplot') {
               updatemplotModuleSeriesBRUSH(handleBrush(ggplotSelectedModulesSeries[['table']],
                                                        input$brush_mplotModuleSeries,
                                                        input$mcheckboxShowFacetModuleSeries,
                                                        FALSE,"Mean"))
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
  content = function(file) {write_excel_csv(ggplotSelectedModulesSeries[['table']], file, na = "")})

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

    output$mdatatableModuleLookup <- renderDataTable({lookedupMods},options = list(pageLength = 10))
})

observeEvent({
  input$mbuttonModLookupNone
},{
  updateTextInput(session, 'mtextInputModLookup', value = "")
  output$mdatatableModuleLookup <- renderDataTable({NULL})
})

output$mbuttonSaveTableModuleLookup <- downloadHandler(filename = function(){paste0("Module Lookup.csv")},
  content = function(file) {write_excel_csv(lookedupMods, file, na = "")})



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
                                input$radioRibbonBoxCellsSeries, 
                                allData$folder,
                                input$checkboxShowLegendSumCellsSeries,input$checkboxShowLegendAllCellsSeries,input$checkboxShowZeroCellsSeries,
                                input$checkboxShowFacetCellsSeries,input$checkboxShowFacetVaccsSeries,
                                input$radioCellsErrorType, input$checkboxShowGridCellsSeries, input$checkboxShowPointsCellsSeries,
                                input$checkboxFreeYCellsSeries,input$numericNumPanelsCellsSeries, input$radioColoursVaccineCells,
                                input$checkboxShowCellsTreatments,input$checkboxShowDischargeCellsSeries,
                                input$checkboxOverrideVaccineNamesCells, input$textVaccineNamesCells),
         envir = .GlobalEnv)
  
  
  output$plotCellsSeries <- renderPlot({ggplotSelectedCellsSeries[['plot']]} ,res = 72)
  output$plotCellsSeriesSIZE <- renderUI({plotOutput("plotCellsSeries", height = isolate(input$numericPlotCellSeriesSIZEheight))})

  output$datatableCellsSeries <- renderDataTable({ggplotSelectedCellsSeries[['table']]},options = list(pageLength = 10))
  
})

output$buttonPNGplotCellsSeries <- downloadHandler(filename = function(){paste0("Cell Kinetics.png")},
  content = function(file) {printPlotPNG(ggplotSelectedCellsSeries[['plot']],
                                         file,session$clientData[["output_plotCellsSeries_height"]],session$clientData[["output_plotCellsSeries_width"]],
                                         input$checkboxHiRes_cells)})

output$buttonSaveTableCellsSeries <- downloadHandler(filename = function(){paste0("Cell Kinetics.csv")},
  content = function(file) {write_excel_csv(ggplotSelectedCellsSeries[['table']], file, na = "")})

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
    
  output$cdatatableCytokines <- renderDataTable({cdp$data},options = list(pageLength = 10))
  output$cplotCytokines <- renderPlot({cdp$plot} ,res = 72)
  output$cplotCytokinesSIZE <- renderUI({plotOutput("cplotCytokines", height = isolate(input$cnumberPlotCytokinesSIZEheight))})
})
output$cbuttonPNGplotCytokines <- downloadHandler(filename = function(){paste0("Selected Cytokines.png")},
  content = function(file) {printPlotPNG(cytokinesDataAndPlot$plot,file,
                                         session$clientData[["output_cplotCytokines_height"]],
                                         session$clientData[["output_cplotCytokines_width"]],
                                         input$checkboxHiRes_cytokines)})

output$buttonSaveTableCytokines <- downloadHandler(filename = function(){paste0("Selected Cytokines.csv")},
  content = function(file) {write_excel_csv(cytokinesDataAndPlot$data, file, na = "")})

observeEvent(input$cbuttonAddAllCytokines,{updateSelectInput(session, 'cselectCytokines', selected = sort(unique(cytokines[["Fold Increase"]][["CYTOKINE"]])))})
observeEvent(input$cbuttonAddNoneCytokines,{updateSelectInput(session, 'cselectCytokines', selected = character(0))})
observeEvent(input$cbuttonAddAllCytokineTreats,{updateSelectInput(session, 'cselectTreatments', selected = sort(unique(cytokines[["Fold Increase"]][["ACTARMCD"]])))})
observeEvent(input$cbuttonAddNoneCytokineTreats,{updateSelectInput(session, 'cselectTreatments', selected = character(0))})
observeEvent(input$cbuttonAddAllCytokineDays,{updateSelectInput(session, 'cselectDays', selected = sort(unique(cytokines[["Fold Increase"]][["DAY"]])))})
observeEvent(input$cbuttonAddNoneCytokineDays,{updateSelectInput(session, 'cselectDays', selected = character(0))})


  #################### End Of Server #########################

show("hiddenDiv")

} # end of server
