
server <- function(input, output, session) {
  #   #################### Initial Setup #########################
  is_local <- Sys.getenv('SHINY_PORT') == ""
  
  if(is_local == TRUE) {
    hideTab(inputId = "navbarTop", target = "Load data")
    hideTab(inputId = "navbarTop", target = "ReadMe")
  } else {
    hideTab(inputId = "navbarTop", target = "Password")
  }
  hideTab(inputId = "navbarTop", target = "Explore By Probe")
  hideTab(inputId = "navbarTop", target = "Explore By Module")
  hideTab(inputId = "navProbe", target = "Selected Probes")
  hideTab(inputId = "navProbe", target = "Probes:Series")
  hideTab(inputId = "navProbe", target = "Genes->Modules")
  hideTab(inputId = "navProbe", target = "Modules")
  hideTab(inputId = "navProbe", target = "Module->Genes")
  hideTab(inputId = "navProbe", target = "Modules:Series")
  hideTab(inputId = "navModule", target = "Selected Modules")
  hideTab(inputId = "navModule", target = "Modules:Series")
  
#   #################### Password #########################
  password <- read_rds("p")
  observeEvent(input$buttonPassword, {
    if (input$password == password) {
      showTab(inputId = "navbarTop", target = "Load data")
      showTab(inputId = "navbarTop", target = "ReadMe")
      hideTab(inputId = "navbarTop", target = "Password")
    }
    })
  
  
#   #################### Loading data #########################
#   
  # list local data files on the server
  updateSelectInput(session, 'selectData', choices = basename(list.dirs(path = 'datafiles', recursive = FALSE)))
  
  allData <- reactiveValues(data = NULL,colNames = NULL, folder = NULL,folderpath = NULL, modules = NULL, modulesMeans = NULL)
  observeEvent(input$buttonLoadData, {if (getNewData(allData,input$selectData) == TRUE) {updateLoadControls()}})
  observeEvent(input$fileInputUploadData,{if(loadUploadedData(allData,input$fileInputUploadData,input$textInputUploadFileName)) {updateLoadControls()}})
  
  updateExpressionMinMax <- function(selCol){
    if(!is.null(selCol)){
      expressionValueRange <- getMaxMinValueFromData(allData$data,c(selCol))#allData$colNames
      updateNumericInput(session,'numberExpressionMin',value = expressionValueRange[['Min']])
      updateNumericInput(session,'numberExpressionMax',value = expressionValueRange[['Max']])
    }
  }
  
  updateModuleMinMax <- function(selCol){
    if(!is.null(selCol)){
      expressionValueRange <- getMaxMinValueFromModulesData(allData,c(selCol), input$mcheckboxModuleMedians)
      updateNumericInput(session,'mnumberExpressionMin',value = expressionValueRange[['Min']])
      updateNumericInput(session,'mnumberExpressionMax',value = expressionValueRange[['Max']])
    }
  }
  
  updateLoadControls <- function(){
    
    topGenesAndModules(NULL)
    topModulesSelected(NULL)
    
    # show hide the nav tabs
    showTab(inputId = "navbarTop", target = "Explore By Probe")
    showTab(inputId = "navbarTop", target = "Explore By Module")
    showTab(inputId = "navProbe", target = "Select")
    hideTab(inputId = "navProbe", target = "Selected Probes")
    hideTab(inputId = "navProbe", target = "Probes:Series")
    hideTab(inputId = "navProbe", target = "Genes->Modules")
    showTab(inputId = "navProbe", target = "Select")
    hideTab(inputId = "navProbe", target = "Modules")
    hideTab(inputId = "navProbe", target = "Module->Genes")
    hideTab(inputId = "navProbe", target = "Modules:Series")
    hideTab(inputId = "navModule", target = "Selected Modules")
    hideTab(inputId = "navModule", target = "Modules:Series")
    
    # these must be updated here as they do not observe allData
    updateSelectInput(session, 'selectColumn', choices = allData$colNames, selected = character(0))
    updateSelectInput(session, 'selectColumnsForSeries', choices = allData$colNames, selected = character(0))
    updateSelectInput(session, 'selectColumnForModuleSeries', choices = allData$colNames, selected = character(0))
    
    output$plotTopGenesSeries <- renderPlot({NULL})
    output$datatableTopGenesSeries <- renderDataTable({NULL})
    output$plotModuleSeries <- renderPlot({NULL})
    output$datatableModuleSeries <- renderDataTable({NULL})

    # modules DO NOT RESPOND. NEED TO FIX
    updateSelectInput(session, 'mselectColumn', choices = allData$colNames, selected = character(0))
    updateSelectInput(session, 'mselectColumnForModuleSeries', choices = allData$colNames, selected = character(0))
    updateSelectInput(session, 'mselectModuleForSeries', choices = character(0))
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
output$datatableAll <- renderDataTable({allData$data},options = list(searching = TRUE))
output$textFileName <- renderText({allData$folder})
output$textFileName2 <- renderText({filtersText()})
output$textFileNameMods <- renderText({modulesAndFiltersText()})

  


#################### PROBES #####################

  #################### Selecting Columns #########################
  # select the genes and identify associated modules
  
  ### selecting events - probes
  observeEvent(
    input$selectColumn,
    {updateExpressionMinMax(input$selectColumn)})
  observeEvent(
    input$buttonResetValuesRangeCol,
    {updateExpressionMinMax(input$selectColumn)})
  observeEvent(
    input$buttonResetValuesRangeData,
    {updateExpressionMinMax(allData$colNames)})
  
  warnedAboutProbeRows <- FALSE
  observeEvent(
    input$checkboxSelectRows,
    {if(warnedAboutProbeRows == FALSE && (input$checkboxSelectRows == FALSE || input$numberGenesStart - input$numberGenesEnd > 100)) {
      warnedAboutProbeRows <<- TRUE
    }})
  
  
  
  ### topGenesAndModules()
  topGenesAndModules <- reactiveVal()
  topGenesAndModules(NULL)
  observeEvent(
    input$buttonApplySelection,
    {
        if(!is.null(input$selectColumn)) {
          if(input$checkboxSelectKeyword == FALSE && input$checkboxSelectValues == FALSE && input$checkboxSelectRows == FALSE) {
            showModal(modalDialog(
              title = "Too Many Rows","You must have at least one filter selected or it will try to return and plot over 65,000 rows."))
            } else {
              # calculate topGenesAndModules()
              geneslist <- getSortedGenesForVaccDay(allData$data,input$selectColumn,input$checkboxDescending,input$checkboxProbesGenes)
              filterText <- ""
              # apply the filters sequentially
              if(input$checkboxSelectKeyword == TRUE){
                geneslist <- getGenesForSearch(geneslist,input$textInputKeyword,input$radioKeywordColumn)
                filterText <- paste0(filterText,'"',input$textInputKeyword,'" in ',input$radioKeywordColumn,' ')
              }
              if(input$checkboxSelectValues == TRUE){
                geneslist <- getGenesForValues(geneslist,input$numberExpressionMin,input$numberExpressionMax)
                filterText <- paste0(filterText,'Value from ',input$numberExpressionMin,' to ',input$numberExpressionMax,' ')
              }
              if(input$checkboxSelectRows == TRUE){
                geneslist <- getGenesForRows(geneslist,input$numberGenesStart,input$numberGenesEnd)
                filterText <- paste0(filterText,'Rows from ',input$numberGenesStart,' to ',input$numberGenesEnd,' ')
              }
              
              if(!is.null(geneslist)) {
                if(nchar(filterText) > 0) {
                  filtersText(
                    paste0(input$selectColumn,' ',filterText,' ',
                           ifelse(input$checkboxDescending == TRUE, ' Sort Descending ',' Sort Ascending '),
                           ifelse(input$checkboxProbesGenes == TRUE, ' Gene Averages ',' Individual Probes ')
                    ))
                  dataAndFiltersText(paste0(allData$folder,': ',filtersText()))
                } else {
                  filtersText(paste0(input$selectColumn,' [No filters] ',ifelse(input$checkboxDescending == TRUE, ' Sort Descending, ',' Sort Ascending, '),ifelse(input$checkboxProbesGenes == TRUE, ' Gene Averages ',' Individual Probes ')))
                  dataAndFiltersText(paste0(allData$folder,': ',filtersText()))
                }
              } else {
                filtersText("")
                dataAndFiltersText("")
              }
    
              ############ lookup the genes and modules
              topGenesAndModules(selectedGenesAndModules(geneslist))
              
              # show the tabs
              showTab(inputId = "navProbe", target = "Selected Probes")
              showTab(inputId = "navProbe", target = "Probes:Series")
              showTab(inputId = "navProbe", target = "Genes->Modules")
              showTab(inputId = "navProbe", target = "Module->Genes")
              showTab(inputId = "navProbe", target = "Modules")
              showTab(inputId = "navProbe", target = "Modules:Series")
            }
      }
    }
  )
  
  
  
  observeEvent(
    topGenesAndModules(),
    {
      # these are non-reactive and need a manual reboot
      output$plotModuleSeries <- renderPlot({NULL})
      output$datatableModuleSeries <- renderDataTable({NULL})
      updateSelectInput(session, 'selectColumnForModuleSeries', selected = NULL)
      updateSelectInput(session, 'selectModuleForSeries', choices = NULL, selected = NULL)
      
      output$plotTopGenesSeries <- renderPlot({NULL})
      output$datatableTopGenesSeries <- renderDataTable({NULL})
      updateSelectInput(session, 'selectColumnsForSeries', selected = NULL)
    }
  )
  
  
  #################### Top Probes #########################
  # output top genes
  output$datatableTopGenesUp <- renderDataTable({
    topGenesAndModules()[['genes']]
  })
  uniquer <- function(){
    sub('\\.','',as.character(as.numeric(Sys.time())))
  }
  output$buttonSaveTableProbes <- downloadTableCSV(topGenesAndModules()[['genes']],'TopGenes_')
  output$buttonSaveListGenes <- downloadGeneList(topGenesAndModules()[['genes']][['Gene']],paste0('TopGenesList_',uniquer()))
  output$buttonSaveListProbes <- downloadGeneList(topGenesAndModules()[['genes']][['Probe']],paste0('TopProbesList_',uniquer()))
  
  #################### Top Probes Series #########################
  observeEvent(
    {
      input$buttonPlotSeries
    },
    {
      topGenesInSeries <- getTopGenesInSeries(allData$data,topGenesAndModules()[['genes']],input$selectColumnsForSeries, 
                        input$checkboxSplitSeries)
      output$datatableTopGenesSeries <- renderDataTable({topGenesInSeries})
      output$buttonSaveTableProbesSeries <- downloadTableCSV(topGenesInSeries,'GenesSeries_')
      
      ggplotTopGenesInSeries <- plotTopGenesInSeries(topGenesInSeries,
                                  input$checkboxConnectSeries,input$checkboxShowLegendSeries,dataAndFiltersText(),input$checkboxSplitSeries,
                                  input$checkboxShowZeroSeries,input$radioBoxLineProbesSeries)
      output$plotTopGenesSeries <- renderPlot({ggplotTopGenesInSeries})
    })
  
  observeEvent(input$buttonAddAllProbesSeries,{updateSelectInput(session, 'selectColumnsForSeries', selected = allData$colNames)})
  observeEvent(input$buttonRemoveAllProbesSeries,{updateSelectInput(session, 'selectColumnsForSeries', selected = character(0))})
  
  
  #################### Genes->Modules #########################
  # output assoc modules
  output$datatableGenesModules <- renderDataTable({
    topGenesAndModules()[['modules']]
  })
  output$buttonSaveTableGenesModules <- downloadTableCSV(topGenesAndModules()[['modules']],'TopGenesModules_')
  
  #################### Modules #########################
  # get the individual gene values for boxplot and the summ stats of the modules
  geneExpressionsForModules <- reactive({
    getExpressionsForModules(topGenesAndModules(),input$selectColumn,allData$data, input$checkboxShowPsuedoModuleGenesModules,filtersText())})
  # draw / save table
  output$datatableSelModulesOnly <- renderDataTable({geneExpressionsForModules()[['summStats']]})
  output$buttonSaveTableModules <- downloadTableCSV(topGenesAndModules()[['modsOnly']],'Modules_')
  
  # draw / save plot
  ggplotGenesModules <-
    reactive({plotGenesModules(geneExpressionsForModules()[['expressions']],dataAndFiltersText(),
                input$checkboxShowLegendGenesModules, input$checkboxShowZeroGenesModules,input$checkboxGGplotGenesModules)})
  output$plotGenesModules <- renderPlot({ggplotGenesModules()})

  #################### Modules->Genes #########################
  # link the module select to the modules for top genes topGenesAndModules()[['modsOnly']]
  mods4Genes <- reactive({moduleDescriptionsForGenes(geneExpressionsForModules()[['summStats']])})
  # change choices in the Genes In Module select based on selected modules
  observeEvent(mods4Genes(),{
    updateSelectInput(session, 'selectModuleForGenes', choices = mods4Genes())
    updateSelectInput(session, 'selectModuleForSeries', choices = mods4Genes())})
  # calculate gene expressions for the module selected
  expressionsInModule <- reactive({getGeneExpressionsInModule(input$selectModuleForGenes,input$selectColumn,
                                                              allData$data,topGenesAndModules()[['genes']])})
  # redraw the table of gene expressions for the module selected
  output$datatableModuleGenes <- renderDataTable({expressionsInModule()})
  output$buttonSaveTableModulesGenes <- downloadTableCSV(expressionsInModule(),'ModuleGenes_')
  
  ggplotModuleGenes <- reactive({plotModuleGenes(expressionsInModule(),isolate(input$selectModuleForGenes),
                                dataAndFiltersText(),input$checkboxShowLegendModuleGenes, input$checkboxShowZeroModuleGenes,
                                input$checkboxGGplotModuleGenes)})
  output$plotModuleGenes <- renderPlot({ggplotModuleGenes()})

  #################### Modules Series #########################
  # selectModuleForSeries and selectColumnForModuleSeries are updated above
  observeEvent({
    input$buttonPlotModuleSeries
  },{
    output$plotModuleSeries <- renderPlot({NULL})
    moduleValues <- getModuleValuesForSeries(allData$data,
      input$selectModuleForSeries,input$selectColumnForModuleSeries, 
      input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries)

    if(!is.null(moduleValues) && input$checkboxShowPseudoModuleModuleSeries == TRUE) {
      moduleValues <- getTopGenesInSeriesToPlotWithModules(allData$data, topGenesAndModules()[['genes']],
                            input$selectColumnForModuleSeries,input$checkboxShowFacetModuleSeries,
                            input$radioRibbonBoxModuleSeries,moduleValues)
    }
    
    output$datatableModuleSeries <- renderDataTable({moduleValues})
    
    ggplotModulesInSeries <-  plotModulesInSeries(moduleValues,dataAndFiltersText(),input$checkboxShowLegendModuleSeries,
        input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries, input$checkboxShowZeroModuleSeries, input$checkboxShowSEModuleSeries)
    output$plotModuleSeries <- renderPlot({ggplotModulesInSeries})
  })
  
  observeEvent(input$buttonAddAllColumnsModuleSeries,{updateSelectInput(session, 'selectColumnForModuleSeries', selected = allData$colNames)})
  observeEvent(input$buttonAddAllModulesModuleSeries,{updateSelectInput(session, 'selectModuleForSeries', selected = mods4Genes())})
  observeEvent(input$buttonRemoveAllColumnsModuleSeries,{updateSelectInput(session, 'selectColumnForModuleSeries', selected = character(0))})
  observeEvent(input$buttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'selectModuleForSeries', selected = character(0))})
  
  
#################### Selecting Columns For Modules #########################
# selecting By Module

observeEvent(
  {input$mselectColumn
    input$mcheckboxModuleMedians},
  {updateModuleMinMax(input$mselectColumn)})
observeEvent(
  input$mbuttonResetValuesRangeCol,
  {updateModuleMinMax(input$mselectColumn)})
observeEvent(
  input$mbuttonResetValuesRangeData,
  {updateModuleMinMax(allData$colNames)})

topModulesSelected <- reactiveVal()
topModulesSelected(NULL)
observeEvent(
  input$mbuttonApplySelection,
  {
    isolate({
      if(!is.null(input$mselectColumn)) {
        # calculate topGenesAndModules()
        if(input$mcheckboxSelectKeyword == FALSE && input$mcheckboxSelectValues == FALSE && input$mcheckboxSelectRows == FALSE) {
          showModal(modalDialog(
            title = "Too Many Modules","You must have at least one filter selected or it will try to return and plot over 600 modules."))
        } else {
          mods <- getSortedModulesForVaccDay(allData$modulesMeans,input$mselectColumn,input$mcheckboxDescending,input$mcheckboxModuleMedians)
          
          filterText <- ""
          # apply the filters sequentially
          if(input$mcheckboxSelectKeyword == TRUE){
            mods <- getModulesForSearch(mods,input$mtextInputKeyword,input$mradioKeywordColumn)
            filterText <- paste0(filterText,'"',input$mtextInputKeyword,'" in ',input$mradioKeywordColumn,' ')
          }
          if(input$mcheckboxSelectValues == TRUE){
            mods <- getModulesForValues(mods,input$mnumberExpressionMin,input$mnumberExpressionMax,input$mcheckboxModuleMedians)
            filterText <- paste0(filterText,'Value from ',input$mnumberExpressionMin,' to ',input$mnumberExpressionMax,' ')
          }
          if(input$mcheckboxSelectRows == TRUE){
            mods <- getModulesForRows(mods,input$mnumberModsStart,input$mnumberModsEnd)
            filterText <- paste0(filterText,'Rows from ',input$mnumberModsStart,' to ',input$mnumberModsEnd,' ')
          }
      
          if(!is.null(mods)) {
            if(nchar(filterText) > 0) {
              modulesAndFiltersText(
                paste0(input$mselectColumn,' ',filterText,' ',
                       ifelse(input$mcheckboxDescending == TRUE, ' Sort Descending ',' Sort Ascending '),
                       ifelse(input$mcheckboxModuleMedians == TRUE, ' Use Median ',' Use Mean ')
                ))
            } else {
              modulesAndFiltersText(
                paste0(input$mselectColumn,' [No filters] ',
                       ifelse(input$mcheckboxDescending == TRUE, ' Sort Descending, ',' Sort Ascending, '),
                       ifelse(input$mcheckboxModuleMedians == TRUE, ' Use Median ',' Use Mean ')
                ))
            }
          } else {
            modulesAndFiltersText("")
          }
          print(mods)
          topModulesSelected(mods)
          # show tabs
          showTab(inputId = "navModule", target = "Selected Modules")
          showTab(inputId = "navModule", target = "Modules:Series")
        }
      }
    })
  }
)


observeEvent(
  topModulesSelected(),
  {
    # these are non-reactive and need a manual reboot
    output$mplotModuleSeries <- renderPlot({NULL})
    output$mdatatableModuleSeries <- renderDataTable({NULL})
    updateSelectInput(session, 'mselectColumnForModuleSeries', selected = character(0))
    updateSelectInput(session, 'mselectPlottedModuleForSeries', selected = character(0))
    updateSelectInput(session, 'mselectModuleForSeries', choices = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]), selected = NULL)

  }
)


#################### Top Modules Selected #########################
# output top genes
output$mdatatableTopModulesUp <- renderDataTable({topModulesSelected()})
output$mbuttonSaveTableModules <- downloadTableCSV(topModulesSelected(),'TopModules_')
output$mbuttonSaveListTopModules <- downloadTopModuleList(topModulesSelected()[['Module']],'TopModulesList_')
output$mbuttonSaveListTopModuleTitles <- downloadTopModuleList(topModulesSelected()[['Title']],'TopModulesTitlesList_')
output$mbuttonSaveListTopModuleCategory <- downloadTopModuleList(topModulesSelected()[['Category']],'TopModulesCategoriesList_')

#################### Plot Modules Selected #########################
ggplotSelectedModules <-
  reactive({plotSelectedModules(allData$modules,topModulesSelected(),modulesAndFiltersText(), 
    input$mcheckboxShowLegendGenesModules, input$mcheckboxShowZeroGenesModules,input$mcheckboxModuleMedians,input$mradioGroupTitleName,input$mcheckboxGGplotGenesModules)})
output$mplotSelectedModules <- renderPlot({ggplotSelectedModules()})

#################### Plot Modules Selected Series #########################
observeEvent({
  input$mbuttonPlotModuleSeries
},{
  # which modules 'Modules Selected By Filters','All Titles In The Datset', 'All Modules In The Datset
  mods2plot <- 
    switch (input$radioModulesModulesSeries,
          'Modules Selected By Filters' = {mods2plot <- input$mselectModuleForSeries},
          'All Titles In The Datset' = {mods2plot <- getModulesForTitles(input$mselectModuleTitles,allData$modulesMeans)},
          'All Modules In The Datset' = {mods2plot <- input$mselectModuleAllModules},
          {NULL}
    )
  if(is.null(mods2plot) || is.null(input$mselectColumnForModuleSeries)) {
    showNotification("Both Column and Modules must be defined", type = "error", duration = 3)
  } else {
    ggplotSelectedModulesSeries <- plotSelectedModulesSeries(allData,input$mselectColumnForModuleSeries,
      mods2plot,modulesAndFiltersText(),input$mcheckboxShowLegendModuleSeries,
      input$mcheckboxShowZeroModuleSeries,input$mradioRibbonBoxModuleSeries, input$mcheckboxShowFacetModuleSeries,
      input$mcheckboxShowSEModuleSeries, input$mradioGroupTitleNameModuleSeries)
    output$mplotModuleSeries <- renderPlot({ggplotSelectedModulesSeries[['plot']]})
    output$mdatatableModuleSeries <- renderDataTable({ggplotSelectedModulesSeries[['table']]})
  }
})


observeEvent(input$mbuttonAddAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = allData$colNames)})
observeEvent(input$mbuttonRemoveAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = character(0))})

observeEvent(input$mbuttonAddAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleForSeries', selected = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))})
observeEvent(input$mbuttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleForSeries', selected = character(0))})

observeEvent(input$mbuttonRemoveAllModuleTitles,{updateSelectInput(session, 'mselectModuleTitles', selected = character(0))})

observeEvent(input$mbuttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleAllModules', choices = character(0))})

} # end of server
