
server <- function(input, output, session) {
# 
#   #################### Loading data #########################
#   
#   
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
  
  ### topGenesAndModules()
  topGenesAndModules <- reactiveVal()
  topGenesAndModules(NULL)
  observeEvent(
    input$buttonApplySelection,
    {
      isolate({
        if(!is.null(input$selectColumn)) {
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
          # lookup the genes and modules
          topGenesAndModules(selectedGenesAndModules(geneslist))
        }
      })
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
  output$datatableTopGenesUp <- renderDataTable({topGenesAndModules()[['genes']]})
  output$buttonSaveTableProbes <- downloadTableCSV(topGenesAndModules()[['genes']],'TopGenes.csv')
  
  #################### Top Probes Series #########################
  observeEvent(
    {
      input$buttonPlotSeries
    },
    {
      topGenesInSeries <- getTopGenesInSeries(allData$data,
                                              topGenesAndModules()[['genes']],input$selectColumnsForSeries, input$checkboxProbesGenes,input$checkboxSplitSeries)
      output$datatableTopGenesSeries <- renderDataTable({topGenesInSeries})
      output$buttonSaveTableProbesSeries <- downloadTableCSV(topGenesInSeries,'GenesSeries.csv')
      
      ggplotTopGenesInSeries <-  plotTopGenesInSeries(topGenesInSeries,input$checkboxProbesGenes,
                                                      input$checkboxConnectSeries,input$checkboxShowLegendSeries,dataAndFiltersText(),input$checkboxSplitSeries,input$checkboxShowZeroSeries)
      output$plotTopGenesSeries <- renderPlot({ggplotTopGenesInSeries})
      output$buttonSavePlotProbesSeries <- downloadPlotPNG(ggplotTopGenesInSeries,'GenesSeries.png')
    })
  observeEvent(input$buttonAddAllProbesSeries,{updateSelectInput(session, 'selectColumnsForSeries', selected = allData$colNames)})
  observeEvent(input$buttonRemoveAllProbesSeries,{updateSelectInput(session, 'selectColumnsForSeries', selected = character(0))})
  
  
  #################### Genes->Modules #########################
  # output assoc modules
  output$datatableGenesModules <- renderDataTable({topGenesAndModules()[['modules']]})
  output$buttonSaveTableGenesModules <- downloadTableCSV(topGenesAndModules()[['modules']],'TopGenesModules.csv')
  
  #################### Modules #########################
  # get the individual gene values for boxplot and the summ stats of the modules
  geneExpressionsForModules <- reactive({
    getExpressionsForModules(topGenesAndModules()[['modsOnly']],input$selectColumn,allData$data)})
  # draw / save table
  output$datatableSelModulesOnly <- renderDataTable({geneExpressionsForModules()[['summStats']]})
  output$buttonSaveTableModules <- downloadTableCSV(topGenesAndModules()[['modsOnly']],'Modules.csv')
  
  # draw / save plot
  ggplotGenesModules <-
    reactive({plotGenesModules(geneExpressionsForModules()[['expressions']],dataAndFiltersText(),
                               input$checkboxShowLegendGenesModules, input$checkboxShowZeroGenesModules)})
  output$plotGenesModules <- renderPlot({ggplotGenesModules()})
  output$buttonSavePlotModules <- downloadPlotPNG(ggplotGenesModules(),'Modules.png')
  
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
  output$buttonSaveTableModulesGenes <- downloadTableCSV(expressionsInModule(),'ModuleGenes.csv')
  
  ggplotModuleGenes <- reactive({plotModuleGenes(expressionsInModule(),isolate(input$selectModuleForGenes),
                                                 dataAndFiltersText(),input$checkboxShowLegendModuleGenes, input$checkboxShowZeroModuleGenes)})
  output$plotModuleGenes <- renderPlot({ggplotModuleGenes()})
  output$buttonSavePlotModulesGenes <- downloadPlotPNG(ggplotModuleGenes(),'ModuleGenes.png')
  
  #################### Modules Series #########################
  # selectModuleForSeries and selectColumnForModuleSeries are updated above
  observeEvent({
    input$buttonPlotModuleSeries
  },{
    moduleValues <- getModuleValuesForSeries(allData$data,input$selectModuleForSeries,input$selectColumnForModuleSeries, input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries)
    output$datatableModuleSeries <- renderDataTable({moduleValues})
    
    ggplotModulesInSeries <-  plotModulesInSeries(moduleValues,dataAndFiltersText(),input$checkboxShowLegendModuleSeries,
                                input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries, input$checkboxShowZeroModuleSeries, input$checkboxShowSEModuleSeries)
    output$plotModuleSeries <- renderPlot({ggplotModulesInSeries})
    output$buttonSavePlotModulesSeries <- downloadPlotPNG(ggplotModulesInSeries,'SelGenesModulesSeries.png')
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
            paste0(allData$folder,': ',gsub('_',' ',input$mselectColumn),' [ ',filterText,']',
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
    updateSelectInput(session, 'mselectColumnForModuleSeries', selected = NULL)
    updateSelectInput(session, 'mselectModuleForSeries', choices = paste0(topModulesSelected()[['Module']],' (',topModulesSelected()[['Title']],')'), selected = NULL)

  }
)


#################### Top Modules Selected #########################
# output top genes
output$mdatatableTopModulesUp <- renderDataTable({topModulesSelected()})
output$mbuttonSaveTableModules <- downloadTableCSV(topModulesSelected(),'TopModules.csv')

#################### Plot Modules Selected #########################
ggplotSelectedModules <-
  reactive({plotSelectedModules(allData$modules,topModulesSelected(),modulesAndFiltersText(), 
    input$mcheckboxShowLegendGenesModules, input$mcheckboxShowZeroGenesModules,input$mcheckboxModuleMedians,input$mradioGroupTitleName)})
output$mplotSelectedModules <- renderPlot({ggplotSelectedModules()})
output$mbuttonSavePlotModules <- downloadPlotPNG(ggplotSelectedModules(),'SelectedModules.png')

#################### Plot Modules Selected Series #########################
observeEvent({
  input$mbuttonPlotModuleSeries
},{
  ggplotSelectedModulesSeries <- plotSelectedModulesSeries(allData,input$mselectColumnForModuleSeries,
    input$mselectModuleForSeries,modulesAndFiltersText(),input$mcheckboxShowLegendModuleSeries,
    input$mcheckboxShowZeroModuleSeries,input$mradioRibbonBoxModuleSeries, input$mcheckboxShowFacetModuleSeries,input$mcheckboxShowSEModuleSeries,
    input$mradioGroupTitleNameModuleSeries)
  output$mplotModuleSeries <- renderPlot({ggplotSelectedModulesSeries[['plot']]})
  output$mbuttonSavePlotModulesSeries <- downloadPlotPNG(ggplotSelectedModulesSeries[['plot']],'SelModulesSeries.png')
  output$mdatatableModuleSeries <- renderDataTable({ggplotSelectedModulesSeries[['table']]})
  
})


observeEvent(input$mbuttonAddAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = allData$colNames)})
observeEvent(input$mbuttonAddAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleForSeries', selected = paste0(topModulesSelected()[['Module']],' (',topModulesSelected()[['Title']],')'))})
observeEvent(input$mbuttonRemoveAllColumnsModuleSeries,{updateSelectInput(session, 'mselectColumnForModuleSeries', selected = character(0))})
observeEvent(input$mbuttonRemoveAllModulesModuleSeries,{updateSelectInput(session, 'mselectModuleForSeries', selected = character(0))})




} # <<<<<<<<<<<< end of server do not go below!