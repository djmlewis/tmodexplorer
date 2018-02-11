# Define UI
ui <- 
  fluidPage(
######################  TABS  #########
  tabsetPanel(type = 'pills',
#################### Data Load #########################
tabPanel('Load data',
         h4("Select pre-loaded dataset to analyse or upload new dataset"),
         fluidRow(
           column(6,
                  wellPanel(
                    selectInput('selectData', NULL, character(0)),
                    actionButton(
                      'buttonLoadData',
                      label = 'Load',
                      style = "background-color: #d4fb78;"
                    )
                  )),
           column(6,
                  wellPanel(
                    textInput(
                      'textInputUploadFileName',
                      label = NULL,
                      placeholder = 'Dataset Name'
                    ),
                    fileInput(
                      'fileInputUploadData',
                      label = NULL,
                      placeholder = 'Choose two files named "data.rds" & "annotation.rds"',
                      buttonLabel = 'Upload…',
                      multiple = TRUE,
                      accept = c(".rds")
                    )
                  ))),
         h3(textOutput('textFileName')),
         dataTableOutput('datatableAll')
),
# ############## PROBES #################
    tabPanel('Explore By Probe',
      h3(textOutput('textFileName2')),
       wellPanel(
         tabsetPanel(type = 'pills',
                     #################### Selecting  ################
                     tabPanel('Select',
                              wellPanel(
                                actionButton('buttonApplySelection','Click To Apply Selections Below',style = "background-color: #d4fb78;"),
                                p(),
                                inputPanel(
                                  selectInput('selectColumn', 'Column To Sort:', character(0), width = 400, selectize = F),
                                  checkboxInput('checkboxDescending', 'Sort Descending', value = TRUE),
                                  checkboxInput('checkboxProbesGenes', 'Gene Averages', value = FALSE)
                                ),
                                h4('Filter probes by a combination of:'),
                                h6('Selected filters are applied in order left to right'),
                                inputPanel(
                                  wellPanel(
                                    checkboxInput('checkboxSelectKeyword', h5('1. Using regex'), value = FALSE),
                                    textInput('textInputKeyword',NULL),
                                    h5("Search:"),
                                    radioButtons('radioKeywordColumn',NULL,choices = c('Description','Gene'))
                                  ),
                                  wellPanel(
                                    checkboxInput('checkboxSelectValues', h5('2. Sorted Column Values Within Range:'), value = FALSE),
                                    numericInput("numberExpressionMin", "Lowest:", value = 0), 
                                    numericInput("numberExpressionMax", "Highest:", value = 0),
                                    actionButton('buttonResetValuesRangeCol','Column'),
                                    actionButton('buttonResetValuesRangeData','Data')
                                  ),
                                  wellPanel(
                                    checkboxInput('checkboxSelectRows', h5('3. Sorted Column Row Numbers'), value = TRUE),
                                    numericInput("numberGenesStart", "From Row:", 0, min = 0, max = 200, step = 5), 
                                    numericInput("numberGenesEnd", "To Row:", 10, min = 0, max = 200, step = 5)
                                  )
                                )
                              )
                     ),
                     #################### Top Probes #######################
                     tabPanel(
                       'Probes',
                       wellPanel(
                         downloadButton('buttonSaveTableProbes', 'Table')
                       ),
                       h4('Selected Probes / Genes'),
                       wellPanel(dataTableOutput('datatableTopGenesUp'))
                     ),
                     #################### Top Probes Series ################
                     tabPanel(
                       'Probes:Series',
                       inputPanel(
                         downloadButton('buttonSaveTableProbesSeries', 'Table'),
                         downloadButton('buttonSavePlotProbesSeries', 'Plot')
                       ),
                       wellPanel(
                         fluidRow(
                           column(1,
                                  wellPanel(
                                    actionButton('buttonPlotSeries','Plot',style = "background-color: #d4fb78;"),
                                    checkboxInput('checkboxSplitSeries', 'Split', value = TRUE),
                                    checkboxInput('checkboxConnectSeries', 'Connect', value = TRUE),
                                    checkboxInput('checkboxShowLegendSeries', 'Legend', value = TRUE),
                                    checkboxInput('checkboxShowZeroSeries', 'Zero ---', value = TRUE)
                                  )),
                           column(8,
                                  wellPanel(
                                    selectInput('selectColumnsForSeries', label = "Columns", choices = character(0), multiple = TRUE),
                                    div(actionButton('buttonAddAllProbesSeries','All'),
                                        actionButton('buttonRemoveAllProbesSeries','None'))
                                  )
                           )
                         ),
                         plotOutput('plotTopGenesSeries', height = '600px')),
                       wellPanel(dataTableOutput('datatableTopGenesSeries'))
                     ),
                     #################### Genes->Modules ##################
                     tabPanel(
                       'Genes->Modules',
                       wellPanel(
                         downloadButton('buttonSaveTableGenesModules', 'Table')
                       ),
                       h4('Modules Associated With Selected Probes / Genes'),
                       wellPanel(dataTableOutput('datatableGenesModules'))
                     ),
                     #################### Modules #########################
                     tabPanel(
                       'Modules',
                       wellPanel(
                         downloadButton('buttonSavePlotModules', 'Plot'),
                         downloadButton('buttonSaveTableModules', 'Table')
                       ),
                       h4('Expression Values Of Modules Associated With Selected Probes / Genes'),
                       wellPanel(
                         inputPanel(
                           checkboxInput('checkboxShowLegendGenesModules', 'Legend', value = TRUE),
                           checkboxInput('checkboxShowZeroGenesModules', 'Zero ---', value = TRUE)
                         ),
                         plotOutput('plotGenesModules', height = '400px')),
                       wellPanel(dataTableOutput('datatableSelModulesOnly'))
                     ),
                     #################### Modules->Genes ###################
                     tabPanel(
                       'Module->Genes',
                       wellPanel(
                         downloadButton('buttonSavePlotModulesGenes', 'Plot'),
                         downloadButton('buttonSaveTableModulesGenes', 'Table')
                       ),
                       wellPanel(
                         selectInput('selectModuleForGenes', 'Expression Values Of Genes In:', character(0), width = '500px'),
                         inputPanel(
                           checkboxInput('checkboxShowLegendModuleGenes', 'Legend', value = TRUE),
                           checkboxInput('checkboxShowZeroModuleGenes', 'Zero ---', value = TRUE)
                         ),
                         plotOutput('plotModuleGenes', height = '600px')),
                       wellPanel(dataTableOutput('datatableModuleGenes'))
                     ),
                     #################### Modules Series ###################
                     tabPanel(
                       'Modules:Series',
                       inputPanel(
                         downloadButton('buttonSavePlotModulesSeries', 'Plot'),
                         downloadButton('buttonSaveTableModulesSeries', 'Table')
                       ),
                       fluidRow(
                         column(1,
                                wellPanel(
                                  actionButton('buttonPlotModuleSeries','Plot',style = "background-color: #d4fb78;"),
                                  radioButtons('radioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Ribbon')),
                                  checkboxInput('checkboxShowFacetModuleSeries', 'Split', value = TRUE),
                                  checkboxInput('checkboxShowLegendModuleSeries', 'Legend', value = TRUE),
                                  checkboxInput('checkboxShowZeroModuleSeries', 'Zero ---', value = TRUE),
                                  checkboxInput('checkboxShowSEModuleSeries', 'Ribbon+SE', value = FALSE)
                                )
                         ),
                         column(5,
                                wellPanel(
                                  selectInput('selectColumnForModuleSeries', label = 'Columns', character(0), multiple = TRUE),
                                  div(actionButton('buttonAddAllColumnsModuleSeries','All'),
                                  actionButton('buttonRemoveAllColumnsModuleSeries','None'))
                                )),
                         column(6,
                                wellPanel(
                                  selectInput('selectModuleForSeries', label = 'Modules', character(0), multiple = TRUE),
                                  div(actionButton('buttonAddAllModulesModuleSeries','All'),
                                  actionButton('buttonRemoveAllModulesModuleSeries','None'))
                                )
                         )
                       ),
                       wellPanel(
                         plotOutput('plotModuleSeries', height = '600px')
                       ),
                       wellPanel(dataTableOutput('datatableModuleSeries'))
                     )
         ) # tabset Panel in explore by probe
       )# wellPanel
  ),
############## MODULES #################
tabPanel('Explore By Module',
         h3(textOutput('textFileNameMods')),
         tabsetPanel(type = 'pills',
    #################### Selecting Modules ################
    tabPanel('Select',
    wellPanel(
      actionButton('mbuttonApplySelection','Click To Apply Selections Below',style = "background-color: #d4fb78;"),
      p(),
      inputPanel(
        selectInput('mselectColumn', 'Column To Sort:', character(0), width = 400, selectize = F),
        checkboxInput('mcheckboxDescending', 'Sort Descending', value = TRUE),
        checkboxInput('mcheckboxModuleMedians', 'Use Medians Not Means', value = FALSE)
      ),
      h4('Filter modules by a combination of:'),
      h6('Selected filters are applied in order left to right'),
      inputPanel(
        wellPanel(
          checkboxInput('mcheckboxSelectKeyword', h5('1. Using regex'), value = FALSE),
          textInput('mtextInputKeyword',NULL),
          h5("Search:"),
          radioButtons('mradioKeywordColumn',NULL,choices = c('Title','Category','Module'))
        ),
        wellPanel(
          checkboxInput('mcheckboxSelectValues', h5('2. Sorted Column Values Within Range:'), value = FALSE),
          numericInput("mnumberExpressionMin", "Lowest:", value = 0), 
          numericInput("mnumberExpressionMax", "Highest:", value = 0),
          actionButton('mbuttonResetValuesRangeCol','Column'),
          actionButton('mbuttonResetValuesRangeData','Data')
        ),
        wellPanel(
          checkboxInput('mcheckboxSelectRows', h5('3. Sorted Column Row Numbers'), value = TRUE),
          numericInput("mnumberModsStart", "From Row:", 0, min = 0, max = 200, step = 5), 
          numericInput("mnumberModsEnd", "To Row:", 10, min = 0, max = 200, step = 5)
        )
      )
    )
   ),
   #################### Top Modules #######################
   tabPanel(
     'Selected Modules',
     wellPanel(
       downloadButton('mbuttonSaveTableModules', 'Table')
     ),
     h4('Selected Modules'),
     wellPanel(dataTableOutput('mdatatableTopModulesUp'))
   ),
   #################### Plot Top Modules #######################
   tabPanel(
     'Plot Selected Modules',
     wellPanel(
       downloadButton('mbuttonSavePlotModules', 'Plot')
     ),
     h4('Expression Values Of Selected Modules'),
     wellPanel(
       inputPanel(
         checkboxInput('mcheckboxShowLegendGenesModules', 'Legend', value = TRUE),
         checkboxInput('mcheckboxShowZeroGenesModules', 'Zero ---', value = TRUE),
         radioButtons('mradioGroupTitleName','Group By',choices = c('Title','Module'),inline = TRUE)
       ),
       plotOutput('mplotSelectedModules', height = '800px'))
   ),
   #################### Top Modules Series #######################
   tabPanel(
     'Modules:Series',
     inputPanel(
       downloadButton('mbuttonSavePlotModulesSeries', 'Plot'),
       downloadButton('mbuttonSaveTableModulesSeries', 'Table')
     ),
     fluidRow(
       column(1,
              wellPanel(
                actionButton('mbuttonPlotModuleSeries','Plot',style = "background-color: #d4fb78;"),
                radioButtons('mradioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Ribbon')),
                checkboxInput('mcheckboxShowFacetModuleSeries', 'Split', value = TRUE),
                checkboxInput('mcheckboxShowLegendModuleSeries', 'Legend', value = TRUE),
                checkboxInput('mcheckboxShowZeroModuleSeries', 'Zero ---', value = TRUE),
                checkboxInput('mcheckboxShowSEModuleSeries', 'Ribbon+SE', value = FALSE),
                radioButtons('mradioGroupTitleNameModuleSeries','Group Boxplot',choices = c('Title','Module'), selected = 'Module')
              )
       ),
       column(5,
              wellPanel(
                selectInput('mselectColumnForModuleSeries', label = 'Columns', character(0), multiple = TRUE),
                div(actionButton('mbuttonAddAllColumnsModuleSeries','All'),
                actionButton('mbuttonRemoveAllColumnsModuleSeries','None'))
              )),
       column(6,
              wellPanel(
                selectInput('mselectModuleForSeries', label = 'Modules', character(0), multiple = TRUE),
                div(actionButton('mbuttonAddAllModulesModuleSeries','All'),
                actionButton('mbuttonRemoveAllModulesModuleSeries','None'))
       )
       )
     ),
     wellPanel(plotOutput('mplotModuleSeries', height = '600px')),
     wellPanel(dataTableOutput('mdatatableModuleSeries'))
   )
 )
    )
  )# top tabset
)# fluidpage
