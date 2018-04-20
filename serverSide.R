
server <- function(input, output, session) {
  
#   #################### Initial Setup #########################
  is_local <- Sys.getenv('SHINY_PORT') == ""
  removeUI(selector = "#myid", immediate = TRUE)
  # initial hidden setup
  if(is_local == FALSE) {
    hideTab(inputId = "navbarTop", target = "Load transcriptomics")
    hideTab(inputId = "navbarTop", target = "ReadMe")
    hideTab(inputId = "navbarTop", target = "Cytokines")
  } else {
    hideTab(inputId = "navbarTop", target = "Password")
  }
  # hide the explores until load
  hideTab(inputId = "navbarTop", target = "Explore By Probe")
  hideTab(inputId = "navbarTop", target = "Explore By Module")
  hideTab(inputId = "navbarTop", target = "Lookup")
  
  # sortCol_Probes <- NULL
  assign("sortCol_Probes",NULL, envir = .GlobalEnv)
  
#   #################### Password #########################
  password <- read_rds("p")
  observeEvent(input$buttonPassword, {
    if (input$password == password) {
      hideTab(inputId = "navbarTop", target = "Password")
      showTab(inputId = "navbarTop", target = "Load transcriptomics", select = TRUE)
      showTab(inputId = "navbarTop", target = "ReadMe")
      showTab(inputId = "navbarTop", target = "Cytokines")
    }
    })
  
  
#   #################### Loading data #########################
#   
  # list local data files on the server
  files <- sort(basename(list.dirs(path = 'datafiles', recursive = FALSE)))
  updatePickerInput(session, 'selectDataFI', choices = list(`Fold Increase From Baseline` = files[grepl("Fold", files)], `Raw Expression Values` = files[!grepl("Fold", files)]))

  allData <- reactiveValues(data = NULL,colNames = NULL, folder = NULL,folderpath = NULL, modules = NULL, modulesMeans = NULL, annot = NULL)
  observeEvent(input$buttonLoadDataFI, {if (getNewData(allData,input$selectDataFI) == TRUE) {updateLoadControls()}},ignoreInit = TRUE)
  observeEvent(input$fileInputUploadData,{if(loadUploadedData(allData,input$fileInputUploadData,input$textInputUploadFileName)) {updateLoadControls()}})
  
  output$buttonsavedatatableAll <- downloadHandler(filename = function(){paste0(allData$folder,".csv")},
    content = function(file) {write.csv(allData$data, file, row.names = FALSE)})
  
  output$datatableAll <- renderDataTable({allData$data},options = list(searching = TRUE))
  
  
  updateExpressionMinMax <- function(selCol){
    if(!is.null(selCol)){
      expressionValueRange <- getMaxMinValueFromData(allData$data,c(selCol))
      updateNumericInput(session,'numberExpressionMin',value = expressionValueRange[['Min']])
      updateNumericInput(session,'numberExpressionMax',value = expressionValueRange[['Max']])
      
      removeNotification(id = "updateExpressionMinMax")
      showNotification(id = "updateExpressionMinMax", paste0("Expression Max Min Reset To ", ifelse(length(selCol)==1,"Selected Column","Whole Dataset"), " Max Min"))
    }
  }
  
  updateModuleMinMax <- function(selCol){
    if(!is.null(selCol) && dataFrameOK(allData$modulesMeans)){
      expressionValueRange <- getMaxMinValueFromModulesData(allData,c(selCol), input$mcheckboxModuleMedians)
      updateNumericInput(session,'mnumberExpressionMin',value = expressionValueRange[['Min']])
      updateNumericInput(session,'mnumberExpressionMax',value = expressionValueRange[['Max']])
      removeNotification(id = "updateModuleMinMax")
      showNotification(id = "updateModuleMinMax", paste0("Module Max Min Reset To ", ifelse(length(selCol)==1,"Selected Column","Whole Dataset"), " Max Min"))
    }
  }
  
  updateLoadControls <- function(){
    
    # show hide the nav tabs to reflect we have loaded data, rehide any needing rehiding post select
    showTab(inputId = "navbarTop", target = "Explore By Probe")
    showTab(inputId = "navbarTop", target = "Explore By Module")
    showTab(inputId = "navbarTop", target = "Lookup")
    
    # we may have been on a different pane so re-select Select
    updateNavbarPage(session,'navProbe',selected = 'Select Probes')
    hideTab(inputId = "navProbe", target = "Selected Probes")
    hideTab(inputId = "navProbe", target = "Probes:Series")
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
    updatePickerInput(session, 'selectColumnDay', choices = vaccDays$days)
    
    
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
    updatePickerInput(session, 'mselectColumnDay', choices = vaccDays$days)
    updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', choices = vaccDays$vaccines, selected = character(0))
    updateSelectInput(session, 'mselectColumnForModuleSeriesDays', choices = vaccDays$days, selected = character(0))
    
    
    updateSelectInput(session, 'mselectPlotModulesInSeries', choices = character(0))
    updatePickerInput(session, 'mselectModuleForGenes', choices = NULL)
    updateSelectInput(session, 'mselectModuleTitles', choices = sort(unique(allData$modulesMeans[['Title']])))
    updateSelectInput(session, 'mselectModuleAllModules', choices = sort(unique(modsNameTitle(allData$modulesMeans[['Module']],allData$modulesMeans[['Title']]))))
    
    # based on menu we just update calc max min as the event is not triggered.
    updateExpressionMinMax(allData$colNames)
    updateModuleMinMax(allData$colNames)
    
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

  ### selecting events - probes
  observeEvent(
    {
      input$selectColumnDay
      input$selectColumnVaccine
    },
    {
      assign("sortCol_Probes",columnsFromVaccinesDays(input$selectColumnVaccine,input$selectColumnDay), envir = .GlobalEnv)
      updateExpressionMinMax(sortCol_Probes)
    })
observeEvent(
    input$buttonResetValuesRangeCol,
    {
      updateExpressionMinMax(sortCol_Probes)})
  observeEvent(
    input$buttonResetValuesRangeData,
    {
      updateExpressionMinMax(allData$colNames)})
  
  # warnedAboutProbeRows <- FALSE
  assign("warnedAboutProbeRows",FALSE, envir = .GlobalEnv)
  observeEvent(
    input$checkboxSelectRows,
    {if(warnedAboutProbeRows == FALSE && (input$checkboxSelectRows == FALSE || input$numberGenesStart - input$numberGenesEnd > 100)) {
      # warnedAboutProbeRows <<- TRUE
      assign("warnedAboutProbeRows",TRUE, envir = .GlobalEnv)
    }})
  
  
  topGenesAndModules <- reactiveVal()
  topGenesAndModules(NULL)
  observeEvent(
    input$buttonApplySelection,
    {
        if(!is.null(sortCol_Probes)) {
          if(input$checkboxSelectKeyword == FALSE && input$checkboxSelectValues == FALSE && input$checkboxSelectRows == FALSE) {
            sendSweetAlert(session, type = 'error', title = "Too Many Probes", text = "You must have at least one filter selected or it will try to return and plot over 60,000 probes")
            } else {
              showNotification("Please wait for filters to be applied…", type = 'message', duration = 3, id = "buttonApplySelection")

                            # show the tabs as we have selected probes
              showTab(inputId = "navProbe", target = "Selected Probes")
              showTab(inputId = "navProbe", target = "Probes:Series")
              showTab(inputId = "navProbe", target = "Genes->Modules")
              showTab(inputId = "navProbe", target = "Modules")
              showTab(inputId = "navProbe", target = "Module->Genes")
              showTab(inputId = "navProbe", target = "Modules:Series")
              
              filterText <- ""
              # apply the filters sequentially, do regex first before gene averages in getSortedGenesForVaccDay strips description
              if(input$checkboxSelectKeyword == TRUE) {
                geneslist <- getGenesForSearch(allData$data,input$textInputKeyword,input$radioKeywordColumn)
                if(dataFrameOK(geneslist)) {
                  filterText <- paste0(filterText,'"',input$textInputKeyword,'" in ',input$radioKeywordColumn,' ')
                  # calculate topGenesAndModules() using geneslist
                  geneslist <- getSortedGenesForVaccDay(geneslist,sortCol_Probes,input$checkboxDescending,input$checkboxProbesGenes)
                }
              } else {
                # calculate topGenesAndModules() using allData
                geneslist <- getSortedGenesForVaccDay(allData$data,sortCol_Probes,input$checkboxDescending,input$checkboxProbesGenes)
              }
              
              if(input$checkboxSelectValues == TRUE && dataFrameOK(geneslist)){
                geneslist <- getGenesForValues(geneslist,input$numberExpressionMin,input$numberExpressionMax)
                filterText <- paste0(filterText,'Value from ',input$numberExpressionMin,' to ',input$numberExpressionMax,' ')
              }
              if(input$checkboxSelectRows == TRUE && dataFrameOK(geneslist)){
                geneslist <- getGenesForRows(geneslist,input$numberGenesStart,input$numberGenesEnd)
                filterText <- paste0(filterText,'Rows from ',input$numberGenesStart,' to ',input$numberGenesEnd,' ')
              }

              if(dataFrameOK(geneslist)) {
                if(nchar(filterText) > 0) {
                  filtersText(
                    paste0(gsub('_',' day ',sortCol_Probes),' ',filterText,' ',
                           ifelse(input$checkboxDescending == TRUE, ' Sort Descending ',' Sort Ascending '),
                           ifelse(input$checkboxProbesGenes == TRUE, ' Gene Averages ',' Individual Probes ')
                    ))
                  dataAndFiltersText(paste0(allData$folder,': ',filtersText()))
                } else {
                  filtersText(paste0(gsub('_',' day ',sortCol_Probes),' [No filters] ',ifelse(input$checkboxDescending == TRUE, ' Sort Descending, ',' Sort Ascending, '),ifelse(input$checkboxProbesGenes == TRUE, ' Gene Averages ',' Individual Probes ')))
                  dataAndFiltersText(paste0(allData$folder,': ',filtersText()))
                }
                show(id = "navProbeHeader")
              } else {
                filtersText("")
                dataAndFiltersText("")
                hide(id = "navProbeHeader")
              }
    
              ############ lookup the genes and modules
              if(dataFrameOK(geneslist)) {
                topGenesAndModules(selectedGenesAndModules(geneslist))
                # show a notifications
                  removeNotification(id = "buttonApplySelection")
                  showNotification(paste0("Found: ",nrow(topGenesAndModules()[['genes']]),
                    ifelse(input$checkboxProbesGenes == TRUE, ' genes', ' probes'), " and ",
                    length(unique(topGenesAndModules()[['modules']][["Module"]])), " modules"), type = 'message')
              } else {
                topGenesAndModules(list(genes = NULL, modules = NULL, modsOnly = NULL))
                removeNotification(id = "buttonApplySelection")
                showNotification(paste0("Found 0 Probes and 0 Modules"), type = 'warning')
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

      updateSelectInput(session, 'selectColumnForModuleSeriesVaccines')
      updateSelectInput(session, 'selectColumnForModuleSeriesDays')
      
      output$plotTopGenesSeries <- renderPlot({NULL})
      output$datatableTopGenesSeries <- renderDataTable({NULL})
      updateSelectInput(session, 'selectVaccinesForSeries')
      updateSelectInput(session, 'selectDaysForSeries')
      # need to determine if Probes or genes
      pgColname <- ifelse('Probe' %in% names(topGenesAndModules()[['genes']]) == FALSE,"Gene","Probe")
      assign("genesOrProbes",pgColname, envir = .GlobalEnv)
      updateSelectInput(session, 'selectGenesProbesForSeries', label = pgColname, choices = topGenesAndModules()[['genes']][[pgColname]], selected = topGenesAndModules()[['genes']][[pgColname]])
      
      if(genesOrProbes == "Gene") {show(id = "checkboxShowProbesOfGenesSeries")} 
      else {hide(id = "checkboxShowProbesOfGenesSeries")}
    }
  )
  
  
  #################### Top Probes #########################
  # output top genes
  output$datatableTopGenesUp <- renderDataTable({topGenesAndModules()[['genes']]})
  output$buttonSaveTableTopGenesUpPlot <- downloadHandler(filename = function(){paste0("Selected Probes-Genes.png")},
    content = function(file) {plotDataTable(topGenesAndModules()[['genes']],file,35)})
  
  dataFilterStr <- function(t) {
    switch (t,
      'g' = return(paste0(allData$folder,'\n# ',filtersText())),
      'm' = return(paste0(allData$folder,'\n# ',modulesAndFiltersText()))
    )
  }
  
  output$buttonSaveTableProbes <- downloadHandler(filename = function(){paste0("Selected Probes-Genes.csv")},
    content = function(file) {write.csv(topGenesAndModules()[['genes']], file, row.names = FALSE)})
  
  output$buttonSaveListGenes <- downloadHandler(filename = function(){paste0("Selected Genes.txt")},
   content = function(file) {write_lines(paste0(paste(unique(topGenesAndModules()[['genes']][['Gene']]), collapse = ','),'\n\n# ',dataFilterStr('g')), file)})
  
  output$buttonSaveListProbes <- downloadHandler(filename = function(){paste0("Selected Probes.txt")},
    content = function(file) {write_lines(paste0(paste(unique(topGenesAndModules()[['genes']][['Probe']]), collapse = ','),'\n\n# ',dataFilterStr('g')), file)})
  

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
      
      output$datatableTopGenesSeries <- renderDataTable({topGenesInSeries})
      
      ggplotTopGenesInSeries <- plotTopGenesInSeries(topGenesInSeries,
        input$checkboxShowPointsSeries,input$checkboxShowSEMSeries,input$checkboxShowLegendSeries,dataAndFiltersText(),input$checkboxSplitSeries,
        input$checkboxShowZeroSeries,input$radioBoxLineProbesSeries,sortCol_Probes, input$checkboxShowGridSeries,input$checkboxShowProbesOfGenesSeries)

      output$plotTopGenesSeries <- renderPlot({ggplotTopGenesInSeries} ,res = 72)
      output$plotTopGenesSeriesSIZE <- renderUI({tagList(conditionalPanel(condition = "input.radioBoxLineProbesSeries != 'Boxplot'",p(style = "text-align: center; color:#b1cd46;","Hover over points to identify")),
                                                         plotOutput("plotTopGenesSeries", height = isolate(input$numberPlotTopGenesSeriesSIZEheight),
                                                        hover = "hover_plotTopGenesSeries"))})
      output$buttonPNGplotTopGenesSeries <- downloadHandler(filename = function(){paste0("Selected Genes As Series.png")},
        content = function(file) {plotPlotPNG(ggplotTopGenesInSeries,file,session$clientData[["output_plotTopGenesSeries_height"]],session$clientData[["output_plotTopGenesSeries_width"]])})
      
    })
  
  observeEvent(
    input$hover_plotTopGenesSeries, 
    {if(input$radioBoxLineProbesSeries != 'Boxplot') handleClick(topGenesInSeries,input$hover_plotTopGenesSeries,"hover_plotTopGenesSeries",input$checkboxSplitSeries,TRUE,"Value")})
  
  observeEvent(input$buttonAddAllVaccinesSeries,{updateSelectInput(session, 'selectVaccinesForSeries', selected = vaccinesDaysFromColNames(allData$colNames)[['vaccines']])})
  observeEvent(input$buttonRemoveAllVaccinesSeries,{updateSelectInput(session, 'selectVaccinesForSeries', selected = character(0))})
  observeEvent(input$buttonAddAllDaysSeries,{updateSelectInput(session, 'selectDaysForSeries', selected = vaccinesDaysFromColNames(allData$colNames)[['days']])})
  observeEvent(input$buttonRemoveAllDaysSeries,{updateSelectInput(session, 'selectDaysForSeries', selected = character(0))})
  observeEvent(input$buttonAddAllGenesProbesSeries,{updateSelectInput(session, 'selectGenesProbesForSeries', choices = topGenesAndModules()[['genes']][[genesOrProbes]], selected = topGenesAndModules()[['genes']][[genesOrProbes]])})
  observeEvent(input$buttonRemoveGenesProbesSeries,{updateSelectInput(session, 'selectGenesProbesForSeries', selected = character(0))})
  
  output$buttonSaveTableProbesSeries <- downloadHandler(filename = function(){paste0("Selected Probes-Genes Series.csv")},
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
    content = function(file) {plotPlotPNG(ggplotGenesModules(),file,session$clientData[["output_plotGenesModules_height"]],session$clientData[["output_plotGenesModules_width"]])})
  
  
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
     content = function(file) {plotPlotPNG(ggplotModuleGenes(),file,session$clientData[["output_plotModuleGenes_height"]],session$clientData[["output_plotModuleGenes_width"]])})
  
  #################### Modules Series #########################
  # selectModuleForSeries and selectColumnForModuleSeries are updated above
  assign("moduleValues",NULL, envir = .GlobalEnv)
  assign("ggplotModulesInSeries",NULL, envir = .GlobalEnv)
  observeEvent({
    input$buttonPlotModuleSeries
  },{
    output$plotModuleSeries <- renderPlot({NULL})
    assign("moduleValues",
           getModuleValuesForSeries(allData$data,
                                    input$selectModuleForSeries,columnsFromVaccinesDays(input$selectColumnForModuleSeriesVaccines,input$selectColumnForModuleSeriesDays), 
                                    input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries), 
           envir = .GlobalEnv)
    
    if(!is.null(moduleValues) && input$checkboxShowPseudoModuleModuleSeries == TRUE) {
      assign("moduleValues",
             getTopGenesInSeriesToPlotWithModules(allData$data, topGenesAndModules()[['genes']],
                                                  columnsFromVaccinesDays(input$selectColumnForModuleSeriesVaccines,input$selectColumnForModuleSeriesDays),input$checkboxShowFacetModuleSeries,
                                                  input$radioRibbonBoxModuleSeries,moduleValues), 
             envir = .GlobalEnv)
    }
    
    output$datatableModuleSeries <- renderDataTable({moduleValues})
    
    assign("ggplotModulesInSeries",
           plotModulesInSeries(moduleValues,dataAndFiltersText(),input$checkboxShowLegendModuleSeries,
                               input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries, input$checkboxShowZeroModuleSeries,
                               input$checkboxShowSEModuleSeries, sortCol_Probes,input$checkboxShowGridModuleSeries, input$checkboxShowPointsModuleSeries), 
           envir = .GlobalEnv)
    
    
    output$plotModuleSeries <- renderPlot({ggplotModulesInSeries} ,res = 72)
    output$plotModuleSeriesSIZE <- renderUI({tagList(conditionalPanel(condition = "input.radioRibbonBoxModuleSeries == 'Lines'",p(style = "text-align: center; color:#b1cd46;","Hover over points to identify")),
      plotOutput("plotModuleSeries", height = isolate(input$numberPlotModuleSeriesSIZEheight), hover = "hover_plotModuleSeries"))})
    
  })
  output$buttonPNGplotModuleSeries <- downloadHandler(filename = function(){paste0("Selected Genes-Modules Series.png")},
    content = function(file) {plotPlotPNG(ggplotModulesInSeries,file,session$clientData[["output_plotModuleSeries_height"]],session$clientData[["output_plotModuleSeries_width"]])})
  
  observeEvent(input$hover_plotModuleSeries, 
               {if(input$radioRibbonBoxModuleSeries != 'Boxplot') handleClick(moduleValues,input$hover_plotModuleSeries,"hover_plotModuleSeries",input$checkboxShowFacetModuleSeries,FALSE,"Value")})
  
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
    assign("lookedupGenes",lookupGenesProbes(input$textInputGeneLookup, allData$annot, input$radioGeneProbeLookup), envir = .GlobalEnv)
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
  

#################### MODULES ####################  
  # sortCol_Mods <- NULL
  assign("sortCol_Mods",NULL, envir = .GlobalEnv)
  
  #################### Selecting Modules ####

  observeEvent({
    input$mselectColumnDay
    input$mselectColumnVaccine
  },
  {
    assign(
      "sortCol_Mods",
      columnsFromVaccinesDays(input$mselectColumnVaccine, input$mselectColumnDay),
      envir = .GlobalEnv
    )
    updateModuleMinMax(sortCol_Mods)
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
          
          filterText <- ""
          # apply the filters sequentially
          if(input$mcheckboxSelectKeyword == TRUE){
            mods <- getModulesForSearch(allData$modulesMeans,input$mtextInputKeyword,input$mradioKeywordColumn)
            if(dataFrameOK(mods)) {
              filterText <- paste0(filterText,'"',input$mtextInputKeyword,'" in ',input$mradioKeywordColumn,' ')
              mods <- getSortedModulesForVaccDay(mods,sortCol_Mods,input$mcheckboxDescending,input$mcheckboxModuleMedians)
            }
          } else {
            mods <- getSortedModulesForVaccDay(allData$modulesMeans,sortCol_Mods,input$mcheckboxDescending,input$mcheckboxModuleMedians)
          }
          
          
          if(input$mcheckboxSelectValues == TRUE && dataFrameOK(mods)){
            mods <- getModulesForValues(mods,input$mnumberExpressionMin,input$mnumberExpressionMax,input$mcheckboxModuleMedians)
            filterText <- paste0(filterText,'Value from ',input$mnumberExpressionMin,' to ',input$mnumberExpressionMax,' ')
          }
          if(input$mcheckboxSelectRows == TRUE && dataFrameOK(mods)){
            mods <- getModulesForRows(mods,input$mnumberModsStart,input$mnumberModsEnd)
            filterText <- paste0(filterText,'Rows from ',input$mnumberModsStart,' to ',input$mnumberModsEnd,' ')
          }
      
          if(dataFrameOK(mods)) {
            if(nchar(filterText) > 0) {
              modulesAndFiltersText(
                paste0(sortCol_Mods,' ',filterText,' ',
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


output$mbuttonSaveListTopModules <- downloadHandler(filename = function(){paste0("Selected By Modules-Modules.txt")},
  content = function(file) {write_lines(paste0(paste(unique(topModulesSelected()[['Module']]), collapse = ','),'\n\n# ',dataFilterStr('m')), file)})
output$mbuttonSaveListTopModuleTitles <- downloadHandler(filename = function(){paste0("Selected By Modules-Titles.txt")},
  content = function(file) {write_lines(paste0(paste(unique(topModulesSelected()[['Title']]), collapse = ','),'\n\n# ',dataFilterStr('m')), file)})
output$mbuttonSaveListTopModuleCategory <- downloadHandler(filename = function(){paste0("Selected By Modules-Categories.txt")},
  content = function(file) {write_lines(paste0(paste(unique(topModulesSelected()[['Category']]), collapse = ','),'\n\n# ',dataFilterStr('m')), file)})



  # Plot Modules Selected #
ggplotSelectedModules <-
  reactive({plotSelectedModules(allData$modules,topModulesSelected(),modulesAndFiltersText(), 
    input$mcheckboxShowLegendGenesModules, input$mcheckboxShowZeroGenesModules,input$mcheckboxModuleMedians,input$mradioGroupTitleName,input$mcheckboxGGplotGenesModules)})
output$mplotSelectedModules <- renderPlot({ggplotSelectedModules()} ,res = 72)
output$mplotSelectedModulesSIZE <- renderUI({plotOutput("mplotSelectedModules", height = input$numbermplotSelectedModulesSIZEheight)})
output$buttonPNGmplotSelectedModules <- downloadHandler(filename = function(){paste0("Selected Modules.png")},
  content = function(file) {plotPlotPNG(ggplotSelectedModules(),file,session$clientData[["output_mplotSelectedModules_height"]],session$clientData[["output_mplotSelectedModules_width"]])})

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
 content = function(file) {plotPlotPNG(ggplotModuleModuleGenes(),file,session$clientData[["output_mplotModuleGenes_height"]],session$clientData[["output_mplotModuleGenes_width"]])})

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
                                     input$mcheckboxShowPointsSeries,sortCol_Mods),
           envir = .GlobalEnv)
    
    
    output$mplotModuleSeries <- renderPlot({ggplotSelectedModulesSeries[['plot']]} ,res = 72)
    output$mplotModuleSeriesSIZE <- renderUI({tagList(conditionalPanel(condition = "input.mradioRibbonBoxModuleSeries == 'Lines'",p(style = "text-align: center; color:#b1cd46;","Hover over points to identify")),
                                                      plotOutput("mplotModuleSeries", height = isolate(input$numbermplotModuleSeriesSIZEheight), hover = "hover_mplotModuleSeries"))})
    output$mdatatableModuleSeries <- renderDataTable({ggplotSelectedModulesSeries[['table']]})
  
})
output$buttonPNGmplotModuleSeries <- downloadHandler(filename = function(){paste0("Selected Modules Series.png")},
  content = function(file) {plotPlotPNG(ggplotSelectedModulesSeries[['plot']],file,session$clientData[["output_mplotModuleSeries_height"]],session$clientData[["output_mplotModuleSeries_width"]])})

observeEvent(input$hover_mplotModuleSeries, 
             {if(input$mradioRibbonBoxModuleSeries != 'Boxplot') handleClick(ggplotSelectedModulesSeries[['table']],input$hover_mplotModuleSeries,"hover_mplotModuleSeries",input$mcheckboxShowFacetModuleSeries,FALSE,"Mean")})

# observeEvent(input$mbuttonAddAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = allData$colNames)})
# observeEvent(input$mbuttonRemoveAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = character(0))})

observeEvent(input$mbuttonAddAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', selected = vaccinesDaysFromColNames(allData$colNames)[['vaccines']])})
observeEvent(input$mbuttonRemoveAllColumnsModuleSeriesVaccines,{updateSelectInput(session, 'mselectColumnForModuleSeriesVaccines', selected = character(0))})
observeEvent(input$mbuttonAddAllColumnsModuleSeriesDays,{updateSelectInput(session, 'mselectColumnForModuleSeriesDays', selected = vaccinesDaysFromColNames(allData$colNames)[['days']])})
observeEvent(input$mbuttonRemoveAllColumnsModuleSeriesDays,{updateSelectInput(session, 'mselectColumnForModuleSeriesDays', selected = character(0))})


observeEvent(input$mbuttonAddAllModuleSeries,{updateSelectInput(session, 'mselectPlotModulesInSeries', selected = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))}) # modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))
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
  assign("lookedupMods",lookupModules(input$mtextInputModLookup, allData$modulesMeans,input$radioArrangeModuleLookupBy), envir = .GlobalEnv)

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

#################### Cytokines #########################
#   load cytokines and update
cytokines <- list(`Fold Increase` = read_rds("cytFoldT.rds"), Concentration = read_rds("cytokinesT.rds"))
updateSelectInput(session, "cselectCytokines", choices = sort(unique(cytokines[["Fold Increase"]][["CYTOKINE"]])))
updateSelectInput(session, "cselectTreatments", choices = sort(unique(cytokines[["Fold Increase"]][["ACTARMCD"]])))
updateSelectInput(session, "cselectDays", choices = sort(unique(cytokines[["Fold Increase"]][["DAY"]])))

cytokinesDataAndPlot <- reactiveValues(data = NULL, plot = NULL)
observeEvent(input$buttonPlotCytokines, {
  cdp <- getCytokinesDataAndPlot(cytokines[[input$cradioCytoMeansRaw]], input$cselectCytokines,
    input$cselectDays, input$cselectTreatments,input$cradioCytokinesWrap,
    input$cradioCytokinesPlotType,input$cradioCytokinesErrorType, input$ccheckboxZoomQuantile, input$ccheckboxFixedY,
    input$ccheckboxOmit0, input$ccheckboxShowN,input$cnumericNumPanels,input$cradioCytoMeansRaw,
    input$ccheckboxShowPoints, input$cradioCytokinesTransformY, input$ccheckboxLogMeans,
    (input$ccheckboxShow1 & input$cradioCytoMeansRaw == 'Fold Increase'))# calc if we plot --- at 1
  cytokinesDataAndPlot$data <- cdp$data
  cytokinesDataAndPlot$plot <- cdp$plot
    
  output$cdatatableCytokines <- renderDataTable({cdp$data})
  output$cplotCytokines <- renderPlot({cdp$plot} ,res = 72)
  output$cplotCytokinesSIZE <- renderUI({plotOutput("cplotCytokines", height = isolate(input$cnumberPlotCytokinesSIZEheight))})
  
})
output$cbuttonPNGplotCytokines <- downloadHandler(filename = function(){paste0("Selected Cytokines.png")},
  content = function(file) {plotPlotPNG(cytokinesDataAndPlot$plot,file,session$clientData[["output_cplotCytokines_height"]],session$clientData[["output_cplotCytokines_width"]])})

output$buttonSaveTableCytokines <- downloadHandler(filename = function(){paste0("Selected Cytokines.csv")},
  content = function(file) {write.csv(cytokinesDataAndPlot$data, file, row.names = FALSE)})

observeEvent(input$cbuttonAddAllCytokines,{updateSelectInput(session, 'cselectCytokines', selected = sort(unique(cytokines[["Fold Increase"]][["CYTOKINE"]])))})
observeEvent(input$cbuttonAddNoneCytokines,{updateSelectInput(session, 'cselectCytokines', selected = character(0))})
observeEvent(input$cbuttonAddAllCytokineTreats,{updateSelectInput(session, 'cselectTreatments', selected = sort(unique(cytokines[["Fold Increase"]][["ACTARMCD"]])))})
observeEvent(input$cbuttonAddNoneCytokineTreats,{updateSelectInput(session, 'cselectTreatments', selected = character(0))})
observeEvent(input$cbuttonAddAllCytokineDays,{updateSelectInput(session, 'cselectDays', selected = sort(unique(cytokines[["Fold Increase"]][["DAY"]])))})
observeEvent(input$cbuttonAddNoneCytokineDays,{updateSelectInput(session, 'cselectDays', selected = character(0))})


  #################### End Of Server #########################
} # end of server
