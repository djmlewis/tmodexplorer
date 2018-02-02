# Define UI
ui <- 
  fluidPage(

######################  TABS  #########

  tabsetPanel(
#################### Data Load #########################
      tabPanel('Load data',
               h4("Select pre-loaded dataset to analyse or upload new dataset"),
               fluidRow(
                 column(5,
                        inputPanel(
                          selectInput('selectData', NULL, character(0)),
                          actionButton(
                            'buttonLoadData',
                            label = 'Load',
                            style = "background-color: #d4fb78;"
                          )
                        )),
                 column(6,
                        inputPanel(
                          textInput(
                            'textInputUploadFileName',
                            label = NULL,
                            placeholder = 'Dataset Name'
                          ),
                          fileInput(
                            'fileInputUploadData',
                            label = NULL,
                            placeholder = 'data.rds & annotation.rds',
                            buttonLabel = 'Upload…',
                            multiple = TRUE,
                            accept = c(".rds")
                          )
                        ))),
               h3(textOutput('textFileName')),
               dataTableOutput('datatableAll')
      ),
############## PROBES #################
      tabPanel(
        'Explore By Probe',
        h3(textOutput('textFileName2')),
      wellPanel(
        tabsetPanel(
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
                  checkboxInput('checkboxSelectKeyword', h5('1. Using Keyword(s) in Gene / Description'), value = FALSE),
                  textInput('textInputKeyword',NULL),
                  h5("Column To Search:"),
                  radioButtons('radioKeywordColumn',NULL,choices = c('Description','Gene'))
                ),
                wellPanel(
                  checkboxInput('checkboxSelectValues', h5('2. Sorted Column Values Within Range:'), value = TRUE),
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
                column(6,selectInput('selectColumnsForSeries', label = 'Add Columns To Series:', choices = character(0), multiple = TRUE)),
                column(1,actionButton('buttonPlotSeries','Plot Series',style = "background-color: #d4fb78;"))
              ),
              inputPanel(
                checkboxInput('checkboxConnectSeries', 'Connect Points', value = TRUE),
                checkboxInput('checkboxShowLegendSeries', 'Show Legend', value = TRUE)
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
              checkboxInput('checkboxShowLegendGenesModules', 'Show Legend', value = TRUE),
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
            fluidRow(column(5, h4(
              'Expression Values Of Genes In:'
            )),
            column(
              7, selectInput('selectModuleForGenes', NULL, character(0))
            )),
            wellPanel(
              checkboxInput('checkboxShowLegendModuleGenes', 'Show Legend', value = TRUE),
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
            inputPanel(
              wellPanel(
                selectInput('selectColumnForModuleSeries', label = 'Columns', character(0), multiple = TRUE),
                actionButton('buttonAddAllColumnsModuleSeries','All')
              ),
              wellPanel(
                selectInput('selectModuleForSeries', label = 'Modules', character(0), multiple = TRUE),
                actionButton('buttonAddAllModulesModuleSeries','All')
              ),
              wellPanel(
              actionButton('buttonPlotModuleSeries','Plot Series',style = "background-color: #d4fb78;"),
              radioButtons('radioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Ribbon'))
              ),
              wellPanel(
                checkboxInput('checkboxShowLegendModuleSeries', 'Legend', value = TRUE),
                checkboxInput('checkboxShowFacetModuleSeries', 'Split', value = TRUE)
              )
            ),
            wellPanel(
              plotOutput('plotModuleSeries', height = '600px')
            ),
            wellPanel(dataTableOutput('datatableModuleSeries'))
          )
        ) # tabset Panel in explore by probe
      )# wellPanel
    ), # tabPanel explore by probe 
############## MODULES #################
    tabPanel('Explore By Module'
    
    )
  )# top tabset
)# fluidpage
