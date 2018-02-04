
server <- function(input, output, session) {
  #################### Loading data #########################
  updateExpressionMinMax <- function(selCol){
    if(!is.null(selCol)){
      expressionValueRange <- getMaxMinValueFromData(allData$data,c(selCol))#allData$colNames
      updateNumericInput(session,'numberExpressionMin',value = expressionValueRange[['Min']])
      updateNumericInput(session,'numberExpressionMax',value = expressionValueRange[['Max']])
    }
  }
  updateLoadControls <- function(){
    # these must be updated here as they do not observe allData
    updateSelectInput(session, 'selectColumn', choices = allData$colNames)
    updateSelectInput(session, 'selectColumnsForSeries', choices = allData$colNames)
    updateSelectInput(session, 'selectColumnForModuleSeries', choices = allData$colNames)
    updateSelectInput(session, 'selectColumnsForSeries', selected = NULL)
    
    output$plotTopGenesSeries <- renderPlot({NULL})
    output$datatableTopGenesSeries <- renderDataTable({NULL})
    output$plotModuleSeries <- renderPlot({NULL})
    output$datatableModuleSeries <- renderDataTable({NULL})
    
    # based on menu we just update calc max min as the event is not triggered.
    updateExpressionMinMax(input$selectColumn)
    # these do resopnd OK outside this scope but put here for neatness
    output$datatableAll <- renderDataTable({allData$data},options = list(searching = TRUE))
    output$textFileName <- renderText({allData$folder})
    output$textFileName2 <- renderText({allData$folder})
  }
  
  observeEvent(
    input$selectColumn,
    {updateExpressionMinMax(input$selectColumn)})
  
  observeEvent(
    input$buttonResetValuesRangeCol,
    {updateExpressionMinMax(input$selectColumn)})
  observeEvent(
    input$buttonResetValuesRangeData,
    {updateExpressionMinMax(allData$colNames)})
  

  allData <- reactiveValues(data = NULL,colNames = NULL, folder = NULL,folderpath = NULL)
  # list local data files on the server
  updateSelectInput(session, 'selectData', choices = basename(list.dirs(path = 'datafiles', recursive = FALSE)))
  observeEvent(input$buttonLoadData, {if (getNewData(allData,input$selectData) == TRUE) {updateLoadControls()}})
  observeEvent(input$fileInputUploadData,{if(loadUploadedData(allData,input$fileInputUploadData,input$textInputUploadFileName)) {updateLoadControls()}})
  
  #################### Selecting Columns #########################
  #store the file name and column
  filenameAndColSelected <- reactive({paste0(allData$folder,': ',gsub('_',' ',input$selectColumn))})
  
  # select the genes and identify associated modules
  topGenesGenesAndModules <- reactive({
    input$buttonApplySelection
    isolate({
      # calculate topGenesGenesAndModules
      geneslist <- getSortedGenesForVaccDay(allData$data,input$selectColumn,input$checkboxDescending,input$checkboxProbesGenes)
      # apply the filters sequentially
      if(input$checkboxSelectKeyword == TRUE){
        geneslist <- getGenesForSearch(geneslist,input$textInputKeyword,input$radioKeywordColumn)
      }
      if(input$checkboxSelectValues == TRUE){
        geneslist <- getGenesForValues(geneslist,input$numberExpressionMin,input$numberExpressionMax)
      }
      if(input$checkboxSelectRows == TRUE){
        geneslist <- getGenesForRows(geneslist,input$numberGenesStart,input$numberGenesEnd)
      }
      # lookup the genes and modules
      selectedGenesAndModules(geneslist)
      })
    })
  observeEvent(
    topGenesGenesAndModules(),
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
  output$datatableTopGenesUp <- renderDataTable({topGenesGenesAndModules()[['genes']]})
  output$buttonSaveTableProbes <- downloadTableCSV(topGenesGenesAndModules()[['genes']],'TopGenes.csv')

  #################### Top Probes Series #########################
  observeEvent(
    {
      input$buttonPlotSeries
    },
    {
      topGenesInSeries <- getTopGenesInSeries(allData$data, 
        topGenesGenesAndModules()[['genes']],input$selectColumnsForSeries, input$checkboxProbesGenes,input$checkboxSplitSeries)
      output$datatableTopGenesSeries <- renderDataTable({topGenesInSeries})
      output$buttonSaveTableProbesSeries <- downloadTableCSV(topGenesInSeries,'GenesSeries.csv')
      
      ggplotTopGenesInSeries <-  plotTopGenesInSeries(topGenesInSeries,input$checkboxProbesGenes,
        input$checkboxConnectSeries,input$checkboxShowLegendSeries,filenameAndColSelected(),input$checkboxSplitSeries,input$checkboxShowZeroSeries)
      output$plotTopGenesSeries <- renderPlot({ggplotTopGenesInSeries})
    output$buttonSavePlotProbesSeries <- downloadPlotPNG(ggplotTopGenesInSeries,'GenesSeries.png')
  })
  observeEvent(input$buttonAddAllProbesSeries,{updateSelectInput(session, 'selectColumnsForSeries', selected = allData$colNames)})
  
  
  #################### Genes->Modules #########################
  # output assoc modules
  output$datatableGenesModules <- renderDataTable({topGenesGenesAndModules()[['modules']]})
  output$buttonSaveTableGenesModules <- downloadTableCSV(topGenesGenesAndModules()[['modules']],'TopGenesModules.csv')

  #################### Modules #########################
  # get the individual gene values for boxplot and the summ stats of the modules
  geneExpressionsForModules <- reactive({
    getExpressionsForModules(topGenesGenesAndModules()[['modsOnly']],input$selectColumn,allData$data)})
  # draw / save table
  output$datatableSelModulesOnly <- renderDataTable({geneExpressionsForModules()[['summStats']]})
  output$buttonSaveTableModules <- downloadTableCSV(topGenesGenesAndModules()[['modsOnly']],'Modules.csv')

    # draw / save plot
  ggplotGenesModules <- 
    reactive({plotGenesModules(geneExpressionsForModules()[['expressions']],filenameAndColSelected(),
    input$checkboxShowLegendGenesModules, input$checkboxShowZeroGenesModules)})
  output$plotGenesModules <- renderPlot({ggplotGenesModules()})
  output$buttonSavePlotModules <- downloadPlotPNG(ggplotGenesModules(),'Modules.png')

  #################### Modules->Genes #########################
  # link the module select to the modules for top genes topGenesGenesAndModules()[['modsOnly']]
  mods4Genes <- reactive({moduleDescriptionsForGenes(geneExpressionsForModules()[['summStats']])})
  # change choices in the Genes In Module select based on selected modules
  observeEvent(mods4Genes(),{
    updateSelectInput(session, 'selectModuleForGenes', choices = mods4Genes())
    updateSelectInput(session, 'selectModuleForSeries', choices = mods4Genes())})
  # calculate gene expressions for the module selected
  expressionsInModule <- reactive({getGeneExpressionsInModule(input$selectModuleForGenes,input$selectColumn,
    allData$data,topGenesGenesAndModules()[['genes']])})
  # redraw the table of gene expressions for the module selected
  output$datatableModuleGenes <- renderDataTable({expressionsInModule()})
  output$buttonSaveTableModulesGenes <- downloadTableCSV(expressionsInModule(),'ModuleGenes.csv')

  ggplotModuleGenes <- reactive({plotModuleGenes(expressionsInModule(),isolate(input$selectModuleForGenes),
      filenameAndColSelected(),input$checkboxShowLegendModuleGenes, input$checkboxShowZeroModuleGenes)})
  output$plotModuleGenes <- renderPlot({ggplotModuleGenes()})
  output$buttonSavePlotModulesGenes <- downloadPlotPNG(ggplotModuleGenes(),'ModuleGenes.png')

  #################### Modules Series #########################
  # selectModuleForSeries and selectColumnForModuleSeries are updated above
  observeEvent({
    input$buttonPlotModuleSeries
  },{
    moduleValues <- getModuleValuesForSeries(allData$data,input$selectModuleForSeries,input$selectColumnForModuleSeries, input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries)
    output$datatableModuleSeries <- renderDataTable({moduleValues})
    
    ggplotModulesInSeries <-  plotModulesInSeries(moduleValues,filenameAndColSelected(),input$checkboxShowLegendModuleSeries,
                                input$radioRibbonBoxModuleSeries,input$checkboxShowFacetModuleSeries, input$checkboxShowZeroModuleSeries)
    output$plotModuleSeries <- renderPlot({ggplotModulesInSeries})
    output$buttonSavePlotModulesSeries <- downloadPlotPNG(ggplotModulesInSeries,'SelGenesModulesSeries.png')
  })
  
  observeEvent(input$buttonAddAllColumnsModuleSeries,{updateSelectInput(session, 'selectColumnForModuleSeries', selected = allData$colNames)})
  observeEvent(input$buttonAddAllModulesModuleSeries,{updateSelectInput(session, 'selectModuleForSeries', selected = mods4Genes())})
}
