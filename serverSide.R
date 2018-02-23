
server <- function(input, output, session) {
# 
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
  output$datatableAll <- renderDataTable({allData$data},options = list(searching = TRUE))
  output$textFileName <- renderText({allData$folder})
  output$textFileName2 <- renderText({dataAndFiltersText()})

  modulesAndFiltersText <- reactiveVal(value = "")
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
      showNotification("If the search returns more than a few hundred rows there will be a long wait!", duration = 5, closeButton = TRUE,
                       id = "notifyRowsProbes", type = "warning")
    }})
  
  
  
  ### topGenesAndModules()
  topGenesAndModules <- reactiveVal()
  topGenesAndModules(NULL)
  observeEvent(
    input$buttonApplySelection,
    {
      isolate({
        if(!is.null(input$selectColumn)) {
          showNotification("Applying selection, please be patient", duration = NULL, closeButton = FALSE,
                           id = "notifyApplySelectionProbes", type = "message")
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
              dataAndFiltersText(
                paste0(allData$folder,': ',gsub('_',' ',input$selectColumn),' ',filterText,' ',
                       ifelse(input$checkboxDescending == TRUE, ' Sort Descending ',' Sort Ascending '),
                       ifelse(input$checkboxProbesGenes == TRUE, ' Gene Averages ',' Individual Probes ')
                ))
            } else {
              dataAndFiltersText(
                paste0(allData$folder,': ',gsub('_',' ',input$selectColumn),' [No filters] ',
                       ifelse(input$checkboxDescending == TRUE, ' Sort Descending, ',' Sort Ascending, '),
                       ifelse(input$checkboxProbesGenes == TRUE, ' Gene Averages ',' Individual Probes ')
                ))
            }
          } else {
            dataAndFiltersText("")
          }
          showNotification("Applying changes ... please be patient",id = "notifyApplySelectionProbes", type = 'message', duration = NULL)
          # lookup the genes and modules
          topGenesAndModules(selectedGenesAndModules(geneslist))
          removeNotification("notifyApplySelectionProbes")
          
        }
      })
    }
  )
  
  
  
  observeEvent(
    topGenesAndModules(),
    {
      showNotification("Updating plots & tables ... please be patient",id = "topGenesAndModules", type = 'message', duration = NULL)
      # these are non-reactive and need a manual reboot
      output$plotModuleSeries <- renderPlot({NULL})
      output$datatableModuleSeries <- renderDataTable({NULL})
      updateSelectInput(session, 'selectColumnForModuleSeries', selected = NULL)
      updateSelectInput(session, 'selectModuleForSeries', choices = NULL, selected = NULL)
      
      output$plotTopGenesSeries <- renderPlot({NULL})
      output$datatableTopGenesSeries <- renderDataTable({NULL})
      updateSelectInput(session, 'selectColumnsForSeries', selected = NULL)
      removeNotification("topGenesAndModules")
    }
  )
  
  
  #################### Top Probes #########################
  # output top genes
  output$datatableTopGenesUp <- renderDataTable({
    showNotification("Updating selected probes datatable ... please be patient",id = "datatableTopGenesUp", type = 'message', duration = NULL)
    t <- topGenesAndModules()[['genes']]
    removeNotification("datatableTopGenesUp")
    t
  })
  output$buttonSaveTableProbes <- downloadTableCSV(topGenesAndModules()[['genes']],'TopGenes_')
  output$buttonSaveListProbes <- downloadGeneList(topGenesAndModules()[['genes']][['Gene']],'TopGenesList_')
  
  #################### Top Probes Series #########################
  
  observeEvent(
    {
      input$buttonPlotSeries
    },
    {
      topGenesInSeries <- getTopGenesInSeries(allData$data,topGenesAndModules()[['genes']],input$selectColumnsForSeries, 
                        input$checkboxProbesGenes,input$checkboxSplitSeries)
      output$datatableTopGenesSeries <- renderDataTable({topGenesInSeries})
      output$buttonSaveTableProbesSeries <- downloadTableCSV(topGenesInSeries,'GenesSeries_')
      
      ggplotTopGenesInSeries <- plotTopGenesInSeries(topGenesInSeries,input$checkboxProbesGenes,
                                  input$checkboxConnectSeries,input$checkboxShowLegendSeries,dataAndFiltersText(),input$checkboxSplitSeries,
                                  input$checkboxShowZeroSeries)
      output$plotTopGenesSeries <- renderPlot({ggplotTopGenesInSeries})
    })
  
  observeEvent(input$buttonAddAllProbesSeries,{updateSelectInput(session, 'selectColumnsForSeries', selected = allData$colNames)})
  observeEvent(input$buttonRemoveAllProbesSeries,{updateSelectInput(session, 'selectColumnsForSeries', selected = character(0))})
  
  
  #################### Genes->Modules #########################
  # output assoc modules
  output$datatableGenesModules <- renderDataTable({
    showNotification("Updating selected probes datatable ... please be patient",id = "datatableGenesModules", type = 'message', duration = NULL)
    t <- topGenesAndModules()[['modules']]
    removeNotification("datatableGenesModules")
    t
  })
  output$buttonSaveTableGenesModules <- downloadTableCSV(topGenesAndModules()[['modules']],'TopGenesModules_')
  
  #################### Modules #########################
  # get the individual gene values for boxplot and the summ stats of the modules
  geneExpressionsForModules <- reactive({
    getExpressionsForModules(topGenesAndModules()[['modsOnly']],input$selectColumn,allData$data)})
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
    moduleValues <- getModuleValuesForSeries(allData$data,input$selectModuleForSeries,input$selectColumnForModuleSeries, input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries)
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
            paste0(allData$folder,': ',gsub('_',' ',input$mselectColumn),' ',filterText,' ',
                   ifelse(input$mcheckboxDescending == TRUE, ' Sort Descending ',' Sort Ascending '),
                   ifelse(input$mcheckboxModuleMedians == TRUE, ' Use Median ',' Use Mean ')
            ))
        } else {
          modulesAndFiltersText(
            paste0(allData$folder,': ',gsub('_',' ',input$mselectColumn),' [No filters] ',
                   ifelse(input$mcheckboxDescending == TRUE, ' Sort Descending, ',' Sort Ascending, '),
                   ifelse(input$mcheckboxModuleMedians == TRUE, ' Use Median ',' Use Mean ')
            ))
        }
      } else {
        modulesAndFiltersText("")
      }
      topModulesSelected(mods)
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

#################### Plot Modules Selected #########################
ggplotSelectedModules <-
  reactive({plotSelectedModules(allData$modules,topModulesSelected(),modulesAndFiltersText(), 
    input$mcheckboxShowLegendGenesModules, input$mcheckboxShowZeroGenesModules,input$mcheckboxModuleMedians,input$mradioGroupTitleName,input$mcheckboxGGplotGenesModules)})
output$mplotSelectedModules <- renderPlot({ggplotSelectedModules()})

#################### Plot Modules Selected Series #########################
observeEvent({
  input$mbuttonPlotModuleSeries
},{
  ggplotSelectedModulesSeries <- plotSelectedModulesSeries(allData,input$mselectColumnForModuleSeries,
    input$mselectPlottedModuleForSeries,modulesAndFiltersText(),input$mcheckboxShowLegendModuleSeries,
    input$mcheckboxShowZeroModuleSeries,input$mradioRibbonBoxModuleSeries, input$mcheckboxShowFacetModuleSeries,input$mcheckboxShowSEModuleSeries,
    input$mradioGroupTitleNameModuleSeries)
  output$mplotModuleSeries <- renderPlot({ggplotSelectedModulesSeries[['plot']]})
  output$mdatatableModuleSeries <- renderDataTable({ggplotSelectedModulesSeries[['table']]})
  
})


observeEvent(input$mbuttonAddAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = allData$colNames)})
observeEvent(input$mbuttonAddAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleForSeries', selected = modsNameTitle(topModulesSelected()[['Module']],topModulesSelected()[['Title']]))})
observeEvent(input$mbuttonRemoveAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = character(0))})
observeEvent(input$mbuttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleForSeries', selected = character(0))})

observeEvent(input$mbuttonRemoveAllModuleTitles,{updateSelectInput(session, 'mselectModuleTitles', selected = character(0))})
observeEvent(input$mbuttonRemoveAllPlottedModulesModuleSeries,{updateSelectInput(session, 'mselectPlottedModuleForSeries', choices = character(0))})
observeEvent(input$mbuttonRemoveAllModules,{updateSelectInput(session, 'mselectModuleAllModules', choices = character(0))})

observeEvent(input$mbuttonSetSelectedModulesAsModuleSeries,{updateSelectInput(session, 'mselectPlottedModuleForSeries', choices = input$mselectModuleForSeries, selected = input$mselectModuleForSeries)})
observeEvent(input$mbuttonAddAllModules,{updateSelectInput(session, 'mselectPlottedModuleForSeries', choices = input$mbuttonAddAllModules, selected = input$mbuttonAddAllModules)})
observeEvent(input$mbuttonAddTitles,{
  cats <- getModulesForTitles(input$mselectModuleTitles,allData$modulesMeans)
  updateSelectInput(session, 'mselectPlottedModuleForSeries',selected = cats, choices = cats)})


} # <<<<<<<<<<<< end of server do not go below!