# Define UI  
ui <- 
  navbarPage(span(style = 'color: #fefc78;','tmodExplorer'), id = 'navbarTop', position = "fixed-top", theme = "theme.css", windowTitle = 'tmodExplorer',
             inverse = TRUE,
             header = tagList(tags$style(type="text/css", "body {padding-top: 70px;}")),
  ######################  TABS  #########
  #################### Password ######################### f8ffeb
  tabPanel('Password',
           h4(style = "text-align: center;",'Please enter the password you have been given to access tmodExplorer'),
           fluidRow(
           column(6, offset = 3,
            wellPanel(style = "background-color: #feffee;",
             fluidRow(
               column(9,passwordInput('password', NULL)),
               column(3,actionButton('buttonPassword','Enter',class = "btn-primary"))
             )
           ))),
           h5(style = "text-align: center;","Please contact d.j.lewis@surrey.ac.uk to request a password")
           
  ),
  #################### Data Load ######################### f8ffeb
  tabPanel('Load data',
    h3("Select pre-loaded dataset to analyse and click 'Load'"),
    fluidRow(
    column(6,
        wellPanel(style = "background-color: #feffee;",
          fluidRow(
            column(10,selectInput('selectDataFI', "Fold Increase From Baseline", character(0))),
            column(2,actionButton('buttonLoadDataFI',label = 'Load',class = "btn-primary"))
      ))),
    column(6,
      wellPanel(style = "background-color: #feffee;",
        fluidRow(
        column(10,selectInput('selectDataRAW', "Raw Expression", character(0))),
        column(2,actionButton('buttonLoadDataRAW',label = 'Load',class = "btn-primary"))
      )))
    ),
    h4(align = 'center', textOutput('textDataName')),
    conditionalPanel(condition = "output.datatableAll != null",
                     downloadButton(class="btn-outline-primary",'buttonsavedatatableAll', 'Table')),
    hr(),
    dataTableOutput('datatableAll')
  ),
  # ############## PROBES #################
      tabPanel('Explore By Probe',
        h4(align = 'center', textOutput('textDataNameProbes')),
         navbarPage(span(style = 'color: #000000;','Probe'), id = 'navProbe', header = h4(align = 'center', textOutput('textFiltersProbes')),
           #################### Selecting  ################
           tabPanel('Select Probe',
                    wellPanel(style = "background-color: #FFFFFF;",
                      # wellPanel(
                        h4("Select a treatment - time column to sort probe values and display responses"),
                        fluidRow(
                          column(2,selectInput('selectColumn', NULL, character(0), selectize = FALSE)),
                          column(2,checkboxInput('checkboxDescending', 'Sort Descending', value = TRUE)),
                          column(2,checkboxInput('checkboxProbesGenes', 'Gene Averages', value = FALSE))
                        ),
                      # ),
                      conditionalPanel(condition = "input.selectColumn != null",
                      h4(align = 'center','Filter probes by a combination of options below. Selected filters are applied in order left to right'),
                      fluidRow(
                          column(4,
                            wellPanel(style = "background-color: #feffee;",
                              h4(checkboxInput('checkboxSelectKeyword', '1. Using regex', value = FALSE)),
                              textInput('textInputKeyword',NULL),
                              h4("Search:"),
                              radioButtons('radioKeywordColumn',NULL,choices = c('Description','Gene','Probe'), inline = TRUE),
                              conditionalPanel(condition = "input.radioKeywordColumn != 'Description'",p(style = "color: #cfdaa2;","Spaces are stripped from Gene and Probe names")),
                              conditionalPanel(condition = "input.radioKeywordColumn == 'Description'",p(style = "color: #cfdaa2;","Spaces are kept in search for Description"))
                          )),
                          column(8,
                            fluidRow(
                              column(6,
                                wellPanel(style = "background-color: #feffee;",
                                  h4(checkboxInput('checkboxSelectValues', '2. Sorted Column Values Within Range:', value = FALSE)),
                                  fluidRow(
                                    column(6,numericInput("numberExpressionMin", "Lowest:", value = 0)), 
                                    column(6,numericInput("numberExpressionMax", "Highest:", value = 0))
                                  ),
                                  actionButton('buttonResetValuesRangeCol','Column', class = 'btn-outline-primary'),
                                  actionButton('buttonResetValuesRangeData','Data', class = 'btn-outline-primary')
                                )
                              ),
                              column(6,
                                wellPanel(style = "background-color: #feffee;",
                                  h4(checkboxInput('checkboxSelectRows', '3. Sorted Column Row Numbers', value = TRUE)),
                                  fluidRow(
                                    column(6,numericInput("numberGenesStart", "From Row:", 0, min = 0, max = 200, step = 5)), 
                                    column(6,numericInput("numberGenesEnd", "To Row:", 10, min = 0, max = 200, step = 5))
                                  ),
                                  p(style = "color: #cfdaa2;", "More than 100 rows will result in slow response")
                                )
                              )
                            )
                          )
                        ),
                      fluidRow(column(4),column(4,actionButton('buttonApplySelection','Apply Selections',class = "btn-primary btn-block")),column(4))
                    )
                    )
           ),
           #################### Top Probes #######################
           tabPanel('Selected Probes',
              hr(),
               h4('Probes Or Genes Meeting The Filters, Sorted By Values In Selected Treatment-Time Column'),
               div(
                downloadButton(class="btn-outline-primary",'buttonSaveTableProbes', 'Table'),
                downloadButton(class="btn-warning",'buttonSaveTableTopGenesUpPlot', 'Table As PNG'),
                downloadButton(class="btn-info",'buttonSaveListGenes', 'Gene List'),
                downloadButton(class="btn-info",'buttonSaveListProbes', 'Probe List')
               ),
             hr(),
               dataTableOutput('datatableTopGenesUp')
           ),
           #################### Top Probes Series ################
           tabPanel('Probes:Series',
             wellPanel(style = "background-color: #FFFFFF",
               h4('Time Course Of Probes Or Genes Meeting The Filters, By Treatment-Time Column'),
               h5("Select Some Treatment-Timepoint Columns And Click Plot"),
               fluidRow(
                 column(3,
                        wellPanel(style = "background-color: #feffee;",
                        conditionalPanel(condition = "input.selectColumnsForSeries != null",
                          actionButton('buttonPlotSeries','Plot',class = "btn-primary btn-block")),
                        conditionalPanel(condition = "input.selectColumnsForSeries == null", p(style = "color: #728f17; text-align: center;","Choose Some Columns To Plot")),
                        fluidRow(
                            column(6,
                                   radioButtons('radioBoxLineProbesSeries',NULL, choices = c('Lines','Boxplot')),
                                   conditionalPanel(condition = "input.radioBoxLineProbesSeries == 'Lines'",
                                    strong(p("Lines options:")),
                                    conditionalPanel(condition = "input.checkboxSplitSeries == true",
                                                     checkboxInput('checkboxShowGridSeries', 'X gridlines', value = TRUE)),
                                    checkboxInput('checkboxShowPointsSeries', 'Points', value = FALSE)
                                   )
                            ),
                            column(6,
                              checkboxInput('checkboxSplitSeries', 'Split', value = TRUE),
                              checkboxInput('checkboxShowLegendSeries', 'Legend', value = FALSE),
                              checkboxInput('checkboxShowZeroSeries', 'Zero', value = TRUE),
                              span(style = "background: green;", sliderInput("numberPlotTopGenesSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500))
                            )),
                        conditionalPanel(condition = "output.plotTopGenesSeries != null",
                        downloadButton(class="btn-warning btn-block",'buttonPNGplotTopGenesSeries', 'Plot As PNG'))
                        )),
                 column(9,
                        wellPanel(style = "background-color: #feffee;",
                          selectInput('selectColumnsForSeries', label = "Click In Box To Select Columns", choices = character(0), multiple = TRUE),
                          div(actionButton('buttonAddAllProbesSeries','All', class="btn-outline-primary"),
                              actionButton('buttonRemoveAllProbesSeries','None'))
                        )
                 )
               ),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotTopGenesSeriesSIZE"))
               ),
               conditionalPanel(condition = "output.datatableTopGenesSeries != null",
                  downloadButton(class="btn-outline-primary", 'buttonSaveTableProbesSeries', 'Table')), hr(), 
               dataTableOutput('datatableTopGenesSeries')),
           #################### Genes->Modules ##################
           tabPanel('Genes->Modules',
               hr(),
               h4('Modules Associated With Selected Probes or Genes'),
               downloadButton(class="btn-outline-primary",'buttonSaveTableGenesModules', 'Table'), 
               hr(), 
               dataTableOutput('datatableGenesModules')),
           #################### Modules #########################
           tabPanel('Modules',
             wellPanel(style = "background-color: #FFFFFF",
               h4('Expression Values Of Modules Associated With Selected Probes / Genes'),
               fluidRow(
                 column(1,checkboxInput('checkboxShowLegendGenesModules', 'Legend', value = FALSE)),
                 column(1,checkboxInput('checkboxShowZeroGenesModules', 'Zero', value = TRUE)),
                 column(2,checkboxInput('checkboxShowPsuedoModuleGenesModules', 'Include Selected As Module', value = TRUE)),
                 column(2,radioButtons('radioGroupProbeModulesBy','Group By',choices = c('Module','Title'),inline = TRUE)),
                 column(1,checkboxInput('checkboxGGplotGenesModules', 'ggplot2', value = FALSE)),
                 column(1,sliderInput("numberPlotGenesModulesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500)),
                 column(2,downloadButton(class="btn-warning",'buttonPNGplotGenesModules', 'Plot As PNG'))
                )
               ),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotGenesModulesSIZE")
               ),
             downloadButton(class="btn-outline-primary",'buttonSaveTableModulesSummary', 'Table'),
             downloadButton(class="btn-outline-primary",'buttonSaveTableModulesRaw', 'Raw Data'),
             downloadButton(class="btn-warning",'buttonSaveTableModulesSummaryPlot', 'Table As PNG'),
             downloadButton(class="btn-warning",'buttonSaveTableModulesSummaryListPlot', 'Modules List As PNG'),
             hr(),
               dataTableOutput('datatableSelModulesOnly')),
           #################### Modules->Genes ###################
           tabPanel('Module->Genes',
              wellPanel(style = "background-color: #FFFFFF;",
              h4('Select A Module From The Menu To View Values Of Its Genes'),
                fluidRow(
                column(4,selectInput('selectModuleForGenes', NULL, character(0), width = '500px')),
              column(8,
               fluidRow(
                 column(2,checkboxInput('checkboxShowLegendModuleGenes', 'Legend', value = FALSE)),
                 column(2,checkboxInput('checkboxShowZeroModuleGenes', 'Zero', value = TRUE)),
                 column(2,checkboxInput('checkboxGGplotModuleGenes', 'ggplot2', value = FALSE)),
                 column(2,sliderInput("numberPlotModuleGenesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500)),
                 column(3,downloadButton(class="btn-warning btn-block",'buttonPNGplotModuleGenes', 'Plot As PNG'))
               ))),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotModuleGenesSIZE"))
               ),

              downloadButton(class="btn-outline-primary",'buttonSaveTableModulesGenes', 'Table'), 
               hr(), 
               dataTableOutput('datatableModuleGenes')),

           #################### Modules Series ###################
           tabPanel('Modules:Series',
            wellPanel(style = "background-color: #FFFFFF;",
              h4('Time Course Of Modules Associated With Selected Probes / Genes'),
              h5('Select Some Treatment-Timepoint Columns And Modules, And Click Plot'),
              fluidRow(
                 column(3,
                        wellPanel(style = "background-color: #feffee;",
                          conditionalPanel(condition = "input.selectColumnForModuleSeries != null && input.selectModuleForSeries != null",
                            actionButton('buttonPlotModuleSeries','Plot',class = "btn-primary btn-block")),
                          conditionalPanel(condition = "input.selectColumnForModuleSeries == null || input.selectModuleForSeries == null", p(style = "color: #728f17; text-align: center;","Choose Columns & Modules To Plot")),
                        fluidRow(
                          column(6,
                          radioButtons('radioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Lines')),
                          conditionalPanel(condition = "input.radioRibbonBoxModuleSeries == 'Lines'",
                                           strong(p("Lines options:")),
                                           conditionalPanel(condition = "input.checkboxShowFacetModuleSeries == true",
                                                            checkboxInput('checkboxShowGridModuleSeries', 'X gridlines', value = TRUE)),
                                           checkboxInput('checkboxShowPointsModuleSeries', 'Points', value = FALSE),
                                           checkboxInput('checkboxShowSEModuleSeries', 'SEM', value = FALSE)
                          )
                          ),
                          column(6,
                                 checkboxInput('checkboxShowFacetModuleSeries', 'Split', value = TRUE),
                                 checkboxInput('checkboxShowLegendModuleSeries', 'Legend', value = FALSE),
                                 checkboxInput('checkboxShowZeroModuleSeries', 'Zero', value = TRUE),
                                 sliderInput("numberPlotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500)
                          )
                        ),
                        conditionalPanel(condition = "output.plotModuleSeries != null",
                        downloadButton(class="btn-warning btn-block",'buttonPNGplotModuleSeries', 'Plot As PNG'))
                      )
                 ),
                 column(4,
                        wellPanel(style = "background-color: #feffee;",
                          selectInput('selectColumnForModuleSeries', label = 'Click In Box To Select Columns', character(0), multiple = TRUE),
                          div(actionButton('buttonAddAllColumnsModuleSeries','All', class="btn-outline-primary"),
                          actionButton('buttonRemoveAllColumnsModuleSeries','None'))
                        )),
                 column(5,
                        wellPanel(style = "background-color: #feffee;",
                          selectInput('selectModuleForSeries', label = 'Click In Box To Select Modules', character(0), multiple = TRUE),
                          fluidRow(
                          column(4,actionButton('buttonAddAllModulesModuleSeries','All', class="btn-outline-primary"),
                            actionButton('buttonRemoveAllModulesModuleSeries','None')),
                          column(8,checkboxInput('checkboxShowPseudoModuleModuleSeries', 'Include Selected As Module', value = TRUE))
                          )
                        )
                 )
               ),
             wellPanel(style = "background-color: #FFFFFF;",
                       uiOutput("plotModuleSeriesSIZE")
             ),
               conditionalPanel(condition = "output.datatableModuleSeries != null",
                downloadButton(class="btn-outline-primary",'buttonSaveTableModulesSeries', 'Table')),
                hr(),
               dataTableOutput('datatableModuleSeries')
           )),
          #################### Gene Lookup ###################
          tabPanel('Gene Lookup',
            wellPanel(style = "background-color: #feffee;",
              h4("Enter a gene name or partial name and click Lookup"),
              h5("Use commas to separate multiple genes. Alternatively, leave box empty and click Lookup to return all genes, then use search boxes above/below table to search"),
              fluidRow(
                column(8,textInput('textInputGeneLookup',NULL)),
                column(4,div(actionButton("buttonGeneLookup", "Lookup",class = "btn-primary"),actionButton("buttonGeneLookupNone", "Clear")))
              )),
              conditionalPanel(condition = "output.datatableGeneLookup != null",
                downloadButton(class="btn-outline-primary",'buttonSaveTableGeneLookup', 'Table')), hr(), 
              dataTableOutput('datatableGeneLookup')
          )
          
) # navProbe
),# explore by probe
  ############## MODULES #################
  tabPanel('Explore By Module',
    h4(align = 'center', textOutput('textDataNameMods')),
    navbarPage(span(style = 'color: #000000;','Module'), id = 'navModule', header = h4(align = 'center', textOutput('textFiltersMods')),
      #################### Selecting Modules ################
      tabPanel('Select Modules',
      wellPanel(style = "background-color: #FFFFFF;",
        # wellPanel(
          h4("Select a treatment - time column to sort module values and display responses"),
          fluidRow(
            column(2, selectInput('mselectColumn', NULL, character(0), width = 400, selectize = F)),
            column(2,checkboxInput('mcheckboxDescending', 'Sort Descending', value = TRUE)),
            column(2,checkboxInput('mcheckboxModuleMedians', 'Use Medians Not Means', value = FALSE))
          ),
        # ),
        conditionalPanel(condition = "input.mselectColumn != null",
        h4(align = 'center','Filter modules by a combination of options below. Selected filters are applied in order left to right'),
        fluidRow(
          column(4,
          wellPanel(style = "background-color: #feffee;",
            h4(checkboxInput('mcheckboxSelectKeyword','1. Using regex', value = FALSE)),
            textInput('mtextInputKeyword',NULL),
            h4("Search:"),
            radioButtons('mradioKeywordColumn',NULL,choices = c('Title','Module'), inline = TRUE),
            conditionalPanel(condition = "input.mradioKeywordColumn == 'Module'",p(style = "color: #cfdaa2;","Spaces are stripped from Module names")),
            conditionalPanel(condition = "input.mradioKeywordColumn == 'Title'",p(style = "color: #cfdaa2;","Spaces are kept in search for Title"))
          )),
          column(8,
           fluidRow(
            column(6,
            wellPanel(style = "background-color: #feffee;",
              h4(checkboxInput('mcheckboxSelectValues', '2. Sorted Column Values Within Range:', value = FALSE)),
            fluidRow(
              column(6,numericInput("mnumberExpressionMin", "Lowest:", value = 0)),
              column(6,numericInput("mnumberExpressionMax", "Highest:", value = 0))
            ),
            actionButton('mbuttonResetValuesRangeCol','Column', class = 'btn-outline-primary'),
            actionButton('mbuttonResetValuesRangeData','Data', class = 'btn-outline-primary')
          )),
          column(6,
            wellPanel(style = "background-color: #feffee;",
              h4(checkboxInput('mcheckboxSelectRows', '3. Sorted Column Row Numbers', value = TRUE)),
              fluidRow(
                column(6,numericInput("mnumberModsStart", "From Row:", 0, min = 0, max = 200, step = 5)),
                column(6,numericInput("mnumberModsEnd", "To Row:", 10, min = 0, max = 200, step = 5))
              ),
              p(style = "color: #cfdaa2;", "More than 100 modules will result in slow response")
            ))
          )
        )# right column wells
        ),
        fluidRow(column(4),column(4,actionButton('mbuttonApplySelection','Apply Selections',class = "btn-primary btn-block")),column(4))
        )
      )
     ),
    #################### Top Modules #######################
     tabPanel('Selected Modules',
       wellPanel(style = "background-color: #FFFFFF;",
         h4('Expression Values Of Selected Modules'),
         fluidRow(
           column(1,checkboxInput('mcheckboxShowLegendGenesModules', 'Legend', value = FALSE)),
           column(1,checkboxInput('mcheckboxShowZeroGenesModules', 'Zero', value = TRUE)),
           column(3,radioButtons('mradioGroupTitleName','Group By',choices = c('Module','Title'),inline = TRUE)),
           column(1,checkboxInput('mcheckboxGGplotGenesModules', 'ggplot2', value = FALSE)),
           column(1,sliderInput("numbermplotSelectedModulesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500)),
           column(2,downloadButton(class="btn-warning",'buttonPNGmplotSelectedModules', 'Plot As PNG'))
         ),
         wellPanel(style = "background-color: #FFFFFF;",
                   uiOutput("mplotSelectedModulesSIZE")
          )
         ),
         div(downloadButton(class="btn-outline-primary",'mbuttonSaveTableModules', 'Table'),
             downloadButton(class="btn-warning",'buttonSaveTableTopModulesUpPlot', 'Table As PNG'),
             downloadButton(class="btn-warning",'buttonSaveTableTopModulesUOnlypPlot', 'Modules List As PNG'),
             downloadButton(class="btn-info",'mbuttonSaveListTopModules', 'Modules List'),
             downloadButton(class="btn-info",'mbuttonSaveListTopModuleTitles', 'Titles List'),
             downloadButton(class="btn-info",'mbuttonSaveListTopModuleCategory', 'Categories List')
         ), hr(), 
         dataTableOutput('mdatatableTopModulesUp')
     ),
    #################### Top Modules Series #######################
     tabPanel('Modules:Series',
       wellPanel(style = "background-color: #FFFFFF;",
         h4("Plot Time Course Of Modules"),
         fluidRow(
           column(3,
                  wellPanel(style = "background-color: #feffee;",
                    conditionalPanel(condition = "input.mselectColumnForModuleSeries != null && 
                      ((input.radioModulesModulesSeries == 'Filters' && input.mselectPlotModulesInSeries != null) || 
                        (input.radioModulesModulesSeries == 'Titles' && input.mselectModuleTitles != null) || 
                        (input.radioModulesModulesSeries == 'Modules' && input.mselectModuleAllModules != null))",
                      actionButton('mbuttonPlotModuleSeries','Plot',class = "btn-primary btn-block")),
                    conditionalPanel(condition = "input.mselectColumnForModuleSeries == null ||
                      ((input.radioModulesModulesSeries == 'Filters' && input.mselectPlotModulesInSeries == null) || 
                        (input.radioModulesModulesSeries == 'Titles' && input.mselectModuleTitles == null) || 
                        (input.radioModulesModulesSeries == 'Modules' && input.mselectModuleAllModules == null))",
                      p(style = "color: #728f17; text-align: center;","Choose Columns & Modules Or Titles To Plot")),
                    fluidRow(
                      column(6,
                        radioButtons('mradioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Lines')),
                        conditionalPanel(condition = "input.mradioRibbonBoxModuleSeries == 'Lines'",
                         strong(p("Lines options:")),
                         conditionalPanel(condition = "input.mcheckboxShowFacetModuleSeries == true",
                                          checkboxInput('mcheckboxShowGridSeries', 'X gridlines', value = TRUE)),
                         checkboxInput('mcheckboxShowPointsSeries', 'Points', value = FALSE),
                         checkboxInput('mcheckboxShowSEModuleSeries', 'SEM', value = FALSE)
                          )
                      ),
                    column(6,
                    radioButtons('mradioGroupTitleNameModuleSeries','Group By:',choices = c('Title','Module'), selected = 'Module'),                    checkboxInput('mcheckboxShowFacetModuleSeries', 'Split', value = TRUE),
                    checkboxInput('mcheckboxShowLegendModuleSeries', 'Legend', value = FALSE),
                    checkboxInput('mcheckboxShowZeroModuleSeries', 'Zero', value = TRUE),
                    sliderInput("numbermplotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500)
                    )),
                    conditionalPanel(condition = "output.mplotModuleSeries != null",
                      downloadButton(class="btn-warning btn-block",'buttonPNGmplotModuleSeries', 'Plot As PNG'))
                  )
           ),
           column(9,
                  wellPanel(style = "background-color: #feffee;",
                    selectInput('mselectColumnForModuleSeries', label = 'Click In Box To Select Columns To Plot', character(0), multiple = TRUE),
                    div(actionButton('mbuttonAddAllColumnsModuleSeries','All', class="btn-outline-primary"),
                    actionButton('mbuttonRemoveAllColumnsModuleSeries','None'))
                    ),
                  div(
                    h5("Select Modules To Plot Using By Clicking In One Of The Boxes Below:"),
                    h6("You cannot paste into these boxes. Paste any saved lists using the Select Modules regex filter"),
                    radioButtons('radioModulesModulesSeries', NULL, inline = TRUE, choiceNames = list(
                      wellPanel(style = "background-color: #feffee;",
                        selectInput('mselectPlotModulesInSeries', label = 'Modules Selected By Filters', character(0), multiple = TRUE),
                        actionButton('mbuttonAddAllModuleSeries','All', class="btn-outline-primary"),
                        actionButton('mbuttonRemoveAllModuleSeries','None')
                      ),
                      wellPanel(style = "background-color: #dcefa0;",
                        selectInput('mselectModuleAllModules', label = 'Modules In Dataset', character(0), multiple = TRUE),
                        fluidRow(
                        column(4,actionButton('mbuttonRemoveAllModulesModuleSeries','None')),
                        column(8,conditionalPanel(condition = "input.mselectModuleAllModules != null",
                        downloadButton(class="btn-info",'mbuttonSaveListTopModulesSeries', 'Modules List')))
                        )
                      ),
                      wellPanel(style = "background-color: #dcefa0;",
                        selectInput('mselectModuleTitles', label = 'Titles In Dataset', character(0), multiple = TRUE),
                        fluidRow(
                        column(4,actionButton('mbuttonRemoveAllModuleTitles','None')),
                        column(8,conditionalPanel(condition = "input.mselectModuleTitles != null",
                        downloadButton(class="btn-info",'mbuttonSaveListTopModuleTitlesSeries', 'Titles List')))
                        )
                      )
                    ),
                    choiceValues = list('Filters', 'Modules','Titles')
                    )
                  )
                )
              ),
         wellPanel(style = "background-color: #FFFFFF;",
          uiOutput("mplotModuleSeriesSIZE")
          # plotOutput('mplotModuleSeries', height = '600px')
         )
       ),
         conditionalPanel(condition = "output.mdatatableModuleSeries != null",
          downloadButton(class="btn-outline-primary",'mbuttonSaveTableModulesSeries', 'Table')), hr(), 
         dataTableOutput('mdatatableModuleSeries')
     ),
    #################### Module Lookup #######################
    tabPanel('Module Lookup',
      wellPanel(style = "background-color: #feffee;",
       h5("Enter a module name and click Lookup"),
       h6("Use commas to separate multiple modules. Alternatively, leave box empty and click Lookup to return all modules, then use search boxes above/below table to search"),
       fluidRow(
         column(5,textInput('mtextInputModLookup',NULL)),
         column(4,div(actionButton("mbuttonModLookup", "Lookup",class = "btn-primary"),
                      actionButton("mbuttonModLookupNone", "Clear"))),
         column(3,radioButtons('radioArrangeModuleLookupBy','Arrange By',choices = c('Module','Title','Category'),inline = TRUE))
       )),
       conditionalPanel(condition = "output.mdatatableModuleLookup != null",
        downloadButton(class="btn-outline-primary",'mbuttonSaveTableModuleLookup', 'Table')), hr(), 
       dataTableOutput('mdatatableModuleLookup')
    )
   )
  ), #explore by module
  ###########   READ ME  ##########
  tabPanel('ReadMe', icon = icon('info-circle'),
           includeHTML("help.html")
  ),
  ##### 
    # ),# top tabset
  hr(),
  div( img(src = 'surrey.png'), img(src = 'ugent.png'), img(src = 'mpiib.png'),img(src = 'icl.png'), img(align = 'right', src = 'eei.png'),img(align = 'right', src = 'biovacsafe.png')),
  hr()
  )# navpage top
