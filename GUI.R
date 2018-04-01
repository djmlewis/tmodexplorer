# Define UI  
ui <-
  navbarPage(span(style = 'color: #fefc78;','tmodExplorer'), id = 'navbarTop', position = "fixed-top", theme = "theme.css", windowTitle = 'tmodExplorer',
             inverse = TRUE,
             header = tagList(tags$style(type="text/css", "body {padding-top: 70px;}")),
  ######################  TABS  #########
  
  #################### Password ################
  tabPanel('Password',
           h4(style = "text-align: center; margin-top: 0px;",'Please enter the password you have been given to access tmodExplorer'),
           fluidRow(
           column(6, offset = 3,
            wellPanel(style = "background-color: #feffee;",
             fluidRow(
               column(9,passwordInput('password', NULL)),
               column(3,actionButton('buttonPassword','Enter',class = "btn-success"))
             )
           ))),
           h5(style = "text-align: center;","Please contact d.j.lewis@surrey.ac.uk to request a password")
           
  ),
  #################### Data Load ######################
  tabPanel('Load transcriptomics',
    h3(style = "text-align: center;","Select pre-loaded transcriptomics dataset to analyse"),
    fluidRow(
    column(8, offset = 2,
        wellPanel(style = "background-color: #feffee;",
          fluidRow(
            column(10,pickerInput(inputId = 'selectDataFI', choices = NULL, options = list(`style` = "btn-success"))),
            column(2,actionButton('buttonLoadDataFI',label = 'Load Data',class = "btn-success"))
      )))
    ),
    h4(style = "text-align: center; margin-top: 0px;",textOutput('textDataName')),
    conditionalPanel(condition = "output.datatableAll != null",
                     downloadButton(class="btn-outline-primary",'buttonsavedatatableAll', 'Table')),
    hr(),
    dataTableOutput('datatableAll')
  ),
  # ############## PROBES #################
      tabPanel('Explore By Probe',
        h4(style = "text-align: center; margin-top: 0px;", textOutput('textDataNameProbes')),
         navbarPage(span(style = 'color: #000000;','Probe'), id = 'navProbe', header = h4(style = "text-align: center; margin-top: 0px;", textOutput('textFiltersProbes')),
           #################### Selecting  ################
           tabPanel('Select Probes',
                    hr(),h3('Apply filters to select probes for plotting. Selected filters are applied in order left → right'),
                    wellPanel(style = "background-color: #FFFFFF;",
                      fluidRow(
                          column(4,
                            conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",
                            wellPanel(style = "background-color: #feffee;",
                              awesomeCheckbox(status = 'success', 'checkboxSelectKeyword', label = h4(style = "margin-top: 0px;",'1. Using regex keyword search'), value = FALSE),
                              textInput('textInputKeyword',NULL),
                              h4(style = "margin-top: 0px;","Search:"),
                              awesomeRadio(status = 'success', 'radioKeywordColumn',NULL,choices = c('Description','Gene','Probe'), inline = TRUE),
                              conditionalPanel(condition = "input.radioKeywordColumn != 'Description'",p(style = "color: #44b84b;","Spaces are stripped from Gene and Probe names")),
                              conditionalPanel(condition = "input.radioKeywordColumn == 'Description'",p(style = "color: #44b84b;","Spaces are kept in search for Description"))
                          ))),
                          column(8,
                            wellPanel(style = "background-color: #ffffff;",
                              h4(style = "text-align: left; margin-top: 0px;", "Select a treatment~time combination to filter by value and sort probes by value"),
                              fluidRow(
                                column(3,pickerInput('selectColumnVaccine', choices = NULL, options = list(`style` = "btn-success"))),
                                column(3,pickerInput('selectColumnDay', choices = NULL, options = list(`style` = "btn-success"))),
                                column(3,awesomeCheckbox(status = 'success', 'checkboxDescending', 'Sort Descending Value', value = TRUE)),
                                column(3,awesomeCheckbox(status = 'success', 'checkboxProbesGenes', 'Use Gene Averages', value = FALSE))
                              ),
                              fluidRow(
                                column(6,
                                  conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",
                                  wellPanel(style = "background-color: #feffee;",
                                    awesomeCheckbox(status = 'success', 'checkboxSelectValues', label = h4(style = "margin-top: 0px;",'2. Values Within Range:'), value = FALSE),
                                    fluidRow(
                                      column(6,numericInput("numberExpressionMin", "Lowest:", value = 0)), 
                                      column(6,numericInput("numberExpressionMax", "Highest:", value = 0))
                                    ),
                                    conditionalPanel(condition = "input.checkboxProbesGenes == true",p(style = "color: #44b84b;","Probes Averaged By Gene Before Applying Limits")),
                                    actionButton('buttonResetValuesRangeCol','Column', class = 'btn-outline-primary'),
                                    actionButton('buttonResetValuesRangeData','Data', class = 'btn-outline-primary')
                                  ))
                                ),
                                column(6,
                                  conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",
                                  wellPanel(style = "background-color: #feffee;",
                                    awesomeCheckbox(status = 'success', 'checkboxSelectRows', label = h4(style = "margin-top: 0px;",'3. Column Row Numbers Within Range:'), value = TRUE),
                                    fluidRow(
                                      column(6,numericInput("numberGenesStart", "From Row:", 0, min = 0, max = NA, step = 5)), 
                                      column(6,numericInput("numberGenesEnd", "To Row:", 10, min = 0, max = NA, step = 5))
                                    ),
                                    conditionalPanel(condition = "input.numberGenesEnd - input.numberGenesStart > 100", p(style = "color: #44b84b;", "More than 100 rows will result in slow response"))
                                  ))
                                )
                              )
                            )
                          )#column
                        )
                    ),
                    conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",fluidRow(column(4,offset = 4, actionButton('buttonApplySelection','Apply Selections',class = "btn-success btn-block")),column(4)))
           ),
           #################### Top Probes #######################
           tabPanel('Selected Probes',
              hr(),
               h4(style = "margin-top: 0px;",'Probes Or Genes Meeting The Filters, Sorted By Values In Selected Treatment-Time Column'),
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
               h4(style = "margin-top: 0px;",'Time Course Of Probes Or Genes Meeting The Filters, By Treatment~Time'),
               h5("Select Some Treatments & Time points And Click Plot"),
               fluidRow(
                 column(3,
                        wellPanel(style = "background-color: #feffee;",
                        conditionalPanel(condition = "input.selectVaccinesForSeries != null && input.selectDaysForSeries != null",
                          actionButton('buttonPlotSeries','Plot',class = "btn-success btn-block")),
                        conditionalPanel(condition = "input.selectVaccinesForSeries == null || input.selectDaysForSeries == null", p(style = "color: #728f17; text-align: center;","Choose Some Columns To Plot")),
                        fluidRow(
                            column(6,
                                   awesomeRadio(status = 'success', 'radioBoxLineProbesSeries', " ", choices = c('Lines','Boxplot')),
                                   conditionalPanel(condition = "input.radioBoxLineProbesSeries == 'Lines'",
                                    strong(p("Lines options:")),
                                    conditionalPanel(condition = "input.checkboxSplitSeries == true",
                                                     awesomeCheckbox(status = 'success', 'checkboxShowGridSeries', 'X gridlines', value = TRUE)),
                                    awesomeCheckbox(status = 'success', 'checkboxShowPointsSeries', 'Points', value = FALSE)
                                   )
                            ),
                            column(6,
                              awesomeCheckbox(status = 'success', 'checkboxSplitSeries', 'Split', value = TRUE),
                              awesomeCheckbox(status = 'success', 'checkboxShowLegendSeries', 'Legend', value = FALSE),
                              awesomeCheckbox(status = 'success', 'checkboxShowZeroSeries', '0 |----', value = TRUE),
                              sliderInput("numberPlotTopGenesSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotTopGenesSeriesSIZEheight", "Plot height")
                            ))
                        )),
                 column(9,
                        wellPanel(style = "background-color: #feffee;",
                                  fluidRow(
                                    column(4,selectInput('selectVaccinesForSeries', label = "Treatment", choices = character(0), multiple = TRUE)),
                                    column(2,div(style = "margin-top: 20px;",actionButton('buttonAddAllVaccinesSeries','All', class="btn-outline-primary"),actionButton('buttonRemoveAllVaccinesSeries','None'))),
                                    column(4,selectInput('selectDaysForSeries', label = "Days", choices = character(0), multiple = TRUE)),
                                    column(2,div(style = "margin-top: 20px;",actionButton('buttonAddAllDaysSeries','All', class="btn-outline-primary"),actionButton('buttonRemoveAllDaysSeries','None')))
                                  )
                        )
                 )
               ),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotTopGenesSeriesSIZE")),
               conditionalPanel(condition = "output.plotTopGenesSeries != null",
                                downloadButton(class="btn-warning",'buttonPNGplotTopGenesSeries', 'HiRes PNG'))
               ),
               conditionalPanel(condition = "output.datatableTopGenesSeries != null",
                  downloadButton(class="btn-outline-primary", 'buttonSaveTableProbesSeries', 'Table')), hr(), 
               dataTableOutput('datatableTopGenesSeries')),
           #################### Genes->Modules ##################
           tabPanel('Genes->Modules',
               hr(),
               h4(style = "margin-top: 0px;",'Modules Associated With Selected Probes or Genes'),
               downloadButton(class="btn-outline-primary",'buttonSaveTableGenesModules', 'Table'), 
               hr(), 
               dataTableOutput('datatableGenesModules')),
           #################### Modules #########################
           tabPanel('Modules',
             wellPanel(style = "background-color: #FFFFFF",
               h4(style = "margin-top: 0px;",'Expression Values Of Modules Associated With Selected Probes / Genes'),
               fluidRow(
                 column(3,awesomeCheckbox(status = 'success', 'checkboxShowPsuedoModuleGenesModules', 'Include Selected As Module', value = TRUE)),
                 column(1,awesomeCheckbox(status = 'success', 'checkboxShowLegendGenesModules', 'Legend', value = FALSE)),
                 column(1,awesomeCheckbox(status = 'success', 'checkboxShowZeroGenesModules', '0 |----', value = TRUE)),
                 column(3,style = "margin-top: 10px;",awesomeRadio(status = 'success', 'radioGroupProbeModulesBy',NULL,choices = c('Module','Title'),inline = TRUE)),
                 column(1,awesomeCheckbox(status = 'success', 'checkboxGGplotGenesModules', 'ggplot2', value = FALSE)),
                 column(2, offset = 1, style = "margin-top: -10px;", sliderInput("numberPlotGenesModulesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotGenesModulesSIZEheight", "Plot height"))
               ),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotGenesModulesSIZE")
               ),
               fluidRow(
               column(1,downloadButton(class="btn-warning",'buttonPNGplotGenesModules', 'HiRes PNG')),
               column(11, p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Probe(s) Mapping To Module Genes Are Summarised"))
               )),
             downloadButton(class="btn-outline-primary",'buttonSaveTableModulesSummary', 'Table'),
             downloadButton(class="btn-outline-primary",'buttonSaveTableModulesRaw', 'Raw Data'),
             downloadButton(class="btn-warning",'buttonSaveTableModulesSummaryPlot', 'Table As PNG'),
             downloadButton(class="btn-warning",'buttonSaveTableModulesSummaryListPlot', 'Modules List As PNG'),
             hr(),
               dataTableOutput('datatableSelModulesOnly')),
           #################### Modules->Genes ###################
           tabPanel('Module->Genes',
              wellPanel(style = "background-color: #FFFFFF;",
              h4(style = "margin-top: 0px;",'Expression Values Of Genes Within Modules Associated With Selected Probes / Genes'),
                fluidRow(
                column(6,pickerInput(inputId = 'selectModuleForGenes', label = NULL, choices = NULL, inline = TRUE,options = list(`style` = "btn-success"))),
                column(1,offset = 2, awesomeCheckbox(status = 'success', 'checkboxShowLegendModuleGenes', 'Legend', value = FALSE)),
                column(1,awesomeCheckbox(status = 'success', 'checkboxShowZeroModuleGenes', '0 |----', value = TRUE)),
                column(1,awesomeCheckbox(status = 'success', 'checkboxGGplotModuleGenes', 'ggplot2', value = FALSE)),
                column(1, style = "margin-top: -10px;", sliderInput("numberPlotModuleGenesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotModuleGenesSIZEheight", "Plot height"))               
                ),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotModuleGenesSIZE")
                ),
              fluidRow(
                column(1,downloadButton(class="btn-warning",'buttonPNGplotModuleGenes', 'HiRes PNG')),
                column(11,p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Probe(s) Mapping To Module Genes Are Summarised"))
              )),
              downloadButton(class="btn-outline-primary",'buttonSaveTableModulesGenes', 'Table'), 
               hr(), 
               dataTableOutput('datatableModuleGenes')),

           #################### Modules Series ###################
           tabPanel('Modules:Series',
            wellPanel(style = "background-color: #FFFFFF;",
              h4(style = "margin-top: 0px;",'Time Course Of Modules Associated With Selected Probes / Genes'),
              h5('Select Some Treatment-Timepoint Columns And Modules, And Click Plot'),
              fluidRow(
                 column(3,
                        wellPanel(style = "background-color: #feffee;",
                          conditionalPanel(condition = "input.selectColumnForModuleSeries != null && input.selectModuleForSeries != null",
                            actionButton('buttonPlotModuleSeries','Plot',class = "btn-success btn-block")),
                          conditionalPanel(condition = "input.selectColumnForModuleSeries == null || input.selectModuleForSeries == null", p(style = "color: #728f17; text-align: center;","Choose Columns & Modules To Plot")),
                        fluidRow(
                          column(6,
                          awesomeRadio(status = 'success', 'radioRibbonBoxModuleSeries'," ",choices = c('Boxplot','Lines')),
                          conditionalPanel(condition = "input.radioRibbonBoxModuleSeries == 'Lines'",
                                           strong(p("Lines options:")),
                                           conditionalPanel(condition = "input.checkboxShowFacetModuleSeries == true",
                                                            awesomeCheckbox(status = 'success', 'checkboxShowGridModuleSeries', 'X gridlines', value = TRUE)),
                                           awesomeCheckbox(status = 'success', 'checkboxShowPointsModuleSeries', 'Points', value = FALSE),
                                           awesomeCheckbox(status = 'success', 'checkboxShowSEModuleSeries', 'SEM', value = FALSE)
                          )
                          ),
                          column(6,
                                 awesomeCheckbox(status = 'success', 'checkboxShowFacetModuleSeries', 'Split', value = TRUE),
                                 awesomeCheckbox(status = 'success', 'checkboxShowLegendModuleSeries', 'Legend', value = FALSE),
                                 awesomeCheckbox(status = 'success', 'checkboxShowZeroModuleSeries', '0 |----', value = TRUE),
                                 sliderInput("numberPlotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotModuleSeriesSIZEheight", "Plot height")
                          )
                        )
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
                          column(6,actionButton('buttonAddAllModulesModuleSeries','All', class="btn-outline-primary"),
                            actionButton('buttonRemoveAllModulesModuleSeries','None')),
                          column(6,awesomeCheckbox(status = 'success', 'checkboxShowPseudoModuleModuleSeries', 'Include Selected As Module', value = TRUE))
                          )
                        )
                 )
               ),
             wellPanel(style = "background-color: #FFFFFF;",
                       uiOutput("plotModuleSeriesSIZE")
             ),
             fluidRow(
              column(1,conditionalPanel(condition = "output.plotModuleSeries != null",downloadButton(class="btn-warning",'buttonPNGplotModuleSeries', 'HiRes PNG'))),
              column(11,p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Probe(s) Mapping To Module Genes Are Summarised"))
              )
            ),
               conditionalPanel(condition = "output.datatableModuleSeries != null",
                downloadButton(class="btn-outline-primary",'buttonSaveTableModulesSeries', 'Table')),
                hr(),
               dataTableOutput('datatableModuleSeries')
           )
          
) # navProbe
),# explore by probe
  ############## MODULES #################
  tabPanel('Explore By Module',
    h4(style = "text-align: center; margin-top: 0px;", textOutput('textDataNameMods')),
    navbarPage(span(style = 'color: #000000;','Module'), id = 'navModule', header = h4(style = "text-align: center; margin-top: 0px;", textOutput('textFiltersMods')),
      #################### Selecting Modules ################
      tabPanel('Select Modules',
      hr(),
      h3('Apply filters to select modules for plotting. Selected filters are applied in order left → right'),
      wellPanel(style = "background-color: #FFFFFF;",
      fluidRow(
        column(4,
               conditionalPanel(condition = "input.mselectColumn != null",
                        wellPanel(style = "background-color: #feffee;",
                         awesomeCheckbox(status = 'success',inputId = 'mcheckboxSelectKeyword',label =  h4(style = "margin-top: 0px;",'1. Using regex keyword search'), value = FALSE),
                         textInput('mtextInputKeyword',NULL),
                         h4(style = "margin-top: 0px;","Search:"),
                         awesomeRadio(status = 'success', 'mradioKeywordColumn',NULL,choices = c('Title','Module'), inline = TRUE),
                         conditionalPanel(condition = "input.mradioKeywordColumn == 'Module'",p(style = "color: #44b84b;","Spaces are stripped from Module name search")),
                         conditionalPanel(condition = "input.mradioKeywordColumn == 'Title'",p(style = "color: #44b84b;","Spaces are kept for Title name search"))
               ))
               ),
        column(8,
        wellPanel(style = "background-color: #FFFFFF;",
            h4(style = "text-align: center; margin-top: 0px;", "Select a treatment~time column to filter by value and sort modules by value"),
            fluidRow(
              column(4, pickerInput(inputId = 'mselectColumn', label = NULL, choices = NULL, options = list(`style` = "btn-success"))),
              column(4,awesomeCheckbox(status = 'success', 'mcheckboxDescending', 'Sort Descending', value = TRUE)),
              column(4,awesomeCheckbox(status = 'success', 'mcheckboxModuleMedians', 'Use Medians Not Means', value = FALSE))
            ),
          fluidRow(
             fluidRow(
              column(6,
              conditionalPanel(condition = "input.mselectColumn != null",
                wellPanel(style = "background-color: #feffee;",
                awesomeCheckbox(status = 'success', inputId = 'mcheckboxSelectValues', label =  h4(style = "margin-top: 0px;",'2. Values Within Range:'), value = FALSE),
              fluidRow(
                column(6,numericInput("mnumberExpressionMin", "Lowest:", value = 0)),
                column(6,numericInput("mnumberExpressionMax", "Highest:", value = 0))
              ),
              conditionalPanel(condition = "input.mcheckboxModuleMedians == true",p(style = "color: #44b84b;","Limits Applied To Module Medians, Not Gene Values")),
              conditionalPanel(condition = "input.mcheckboxModuleMedians == false",p(style = "color: #44b84b;","Limits Applied To Module Means, Not Gene Values")),
              actionButton('mbuttonResetValuesRangeCol','Column', class = 'btn-outline-primary'),
              actionButton('mbuttonResetValuesRangeData','Data', class = 'btn-outline-primary')
            ))),
            column(6,
              conditionalPanel(condition = "input.mselectColumn != null",
                wellPanel(style = "background-color: #feffee;",
                awesomeCheckbox(status = 'success', inputId = 'mcheckboxSelectRows', label =  h4(style = "margin-top: 0px;",'3. Column Row Numbers Within Range:'), value = TRUE),
                fluidRow(
                  column(6,numericInput("mnumberModsStart", "From Row:", 0, min = 0, max = NA, step = 5)),
                  column(6,numericInput("mnumberModsEnd", "To Row:", 10, min = 0, max = NA, step = 5))
                ),
                conditionalPanel(condition = "input.mnumberModsEnd - input.mnumberModsStart > 100", p(style = "color: #44b84b;", "More than 100 rows will result in slow response"))
              )))
            )
          )
        )#wp
        )
      )
      ),
      fluidRow(column(4,offset = 4,conditionalPanel(condition = "input.mselectColumn != null",actionButton('mbuttonApplySelection','Apply Selections',class = "btn-success btn-block"))))
    ), #tab
    #################### Top Modules #######################
     tabPanel('Selected Modules',
       wellPanel(style = "background-color: #FFFFFF;",
         h4(style = "margin-top: 0px;",'Expression Values Of Selected Modules'),
         fluidRow(
           column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowLegendGenesModules', 'Legend', value = FALSE)),
           column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowZeroGenesModules', '0 |----', value = TRUE)),
           column(3,style = "margin-top: 10px;", awesomeRadio(status = 'success', 'mradioGroupTitleName',NULL,choices = c('Module','Title'),inline = TRUE)),
           column(3,awesomeCheckbox(status = 'success', 'mcheckboxGGplotGenesModules', 'ggplot2', value = FALSE)),
           column(2, offset = 2, style = "margin-top: -10px;", sliderInput("numbermplotSelectedModulesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numbermplotSelectedModulesSIZEheight", "Plot height"))
           ),
         wellPanel(style = "background-color: #FFFFFF;",
                   uiOutput("mplotSelectedModulesSIZE")
         ),
         fluidRow(
           column(1, downloadButton(class="btn-warning",'buttonPNGmplotSelectedModules', 'HiRes PNG')),
           column(11,p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Probe(s) Mapping To Module Genes Are Summarised")))
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
         h4(style = "margin-top: 0px;","Plot Time Course Of Modules"),
         fluidRow(
           column(3,
                  wellPanel(style = "background-color: #feffee;",
                    conditionalPanel(condition = "input.mselectColumnForModuleSeries != null && 
                      ((input.radioModulesModulesSeries == 'Filters' && input.mselectPlotModulesInSeries != null) || 
                        (input.radioModulesModulesSeries == 'Titles' && input.mselectModuleTitles != null) || 
                        (input.radioModulesModulesSeries == 'Modules' && input.mselectModuleAllModules != null))",
                      actionButton('mbuttonPlotModuleSeries','Plot',class = "btn-success btn-block")),
                    conditionalPanel(condition = "input.mselectColumnForModuleSeries == null ||
                      ((input.radioModulesModulesSeries == 'Filters' && input.mselectPlotModulesInSeries == null) || 
                        (input.radioModulesModulesSeries == 'Titles' && input.mselectModuleTitles == null) || 
                        (input.radioModulesModulesSeries == 'Modules' && input.mselectModuleAllModules == null))",
                      p(style = "color: #728f17; text-align: center;","Choose Columns & Modules Or Titles To Plot")),
                    fluidRow(
                      column(6,
                        awesomeRadio(status = 'success', 'mradioRibbonBoxModuleSeries'," ",choices = c('Boxplot','Lines')),
                        conditionalPanel(condition = "input.mradioRibbonBoxModuleSeries == 'Lines'",
                         strong(p("Lines options:")),
                         conditionalPanel(condition = "input.mcheckboxShowFacetModuleSeries == true",
                                          awesomeCheckbox(status = 'success', 'mcheckboxShowGridSeries', 'X gridlines', value = TRUE)),
                         awesomeCheckbox(status = 'success', 'mcheckboxShowPointsSeries', 'Points', value = FALSE),
                         awesomeCheckbox(status = 'success', 'mcheckboxShowSEModuleSeries', 'SEM', value = FALSE)
                          )
                      ),
                    column(6,
                    awesomeCheckbox(status = 'success', 'mcheckboxShowLegendModuleSeries', 'Legend', value = FALSE),
                    awesomeCheckbox(status = 'success', 'mcheckboxShowZeroModuleSeries', '0 |----', value = TRUE),
                    awesomeCheckbox(status = 'success', 'mcheckboxShowFacetModuleSeries', 'Split', value = TRUE),
                    awesomeRadio(status = 'success', 'mradioGroupTitleNameModuleSeries','Group By:',choices = c('Title','Module'), selected = 'Module'),
                    sliderInput("numbermplotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numbermplotModuleSeriesSIZEheight", "Plot height")
                    ))
                  )
           ),
           column(9,
                  wellPanel(style = "background-color: #feffee;",
                    selectInput('mselectColumnForModuleSeries', label = 'Click In Box To Select Columns To Plot', character(0), multiple = TRUE),
                    div(actionButton('mbuttonAddAllColumnsModuleSeries','All', class="btn-outline-primary"),
                    actionButton('mbuttonRemoveAllColumnsModuleSeries','None'))
                    ),
                  div(
                    h4(style = "margin-top: 0px;","Select Modules To Plot From One Of The Options Below:"),
                    radioGroupButtons('radioModulesModulesSeries', NULL, 
                                      choiceValues = list('Filters', 'Modules','Titles'),
                                      choiceNames = list(' Modules You Selected With Filters', ' All Modules In Dataset',' All Module Titles In Dataset'),
                                      individual = FALSE, justified = TRUE, status = "success",
                                      checkIcon = list(yes = tags$i(class = "fa fa-circle", 
                                                                    style = "color: gold"), 
                                                       no = tags$i(class = "fa fa-circle-o", 
                                                                   style = "color: white"))
                                      ),
                    conditionalPanel(condition = "input.radioModulesModulesSeries == 'Filters'",
                    wellPanel(style = "background-color: #feffee;",
                        selectInput('mselectPlotModulesInSeries', label = 'Modules Selected By Filters', character(0), multiple = TRUE),
                        actionButton('mbuttonAddAllModuleSeries','All', class="btn-outline-primary"),
                        actionButton('mbuttonRemoveAllModuleSeries','None')
                      )),
                    conditionalPanel(condition = "input.radioModulesModulesSeries == 'Modules'",
                        wellPanel(style = "background-color: #dcefa0;",
                        selectInput('mselectModuleAllModules', label = 'Modules In Dataset', character(0), multiple = TRUE),
                        fluidRow(
                        column(4,actionButton('mbuttonRemoveAllModulesModuleSeries','None')),
                        column(8,conditionalPanel(condition = "input.mselectModuleAllModules != null",
                        downloadButton(class="btn-info",'mbuttonSaveListTopModulesSeries', 'Modules List')))
                        )
                      )),
                    conditionalPanel(condition = "input.radioModulesModulesSeries == 'Titles'",
                        wellPanel(style = "background-color: #dcefa0;",
                        selectInput('mselectModuleTitles', label = 'Titles In Dataset', character(0), multiple = TRUE),
                        fluidRow(
                        column(4,actionButton('mbuttonRemoveAllModuleTitles','None')),
                        column(8,conditionalPanel(condition = "input.mselectModuleTitles != null",
                        downloadButton(class="btn-info",'mbuttonSaveListTopModuleTitlesSeries', 'Titles List')))
                        )
                      )),
                    h5(style = "text-align: center;", "You cannot paste into the boxes above Paste any saved lists into the Select Modules regex filter instead")
                  )
                )
              ),
         wellPanel(style = "background-color: #FFFFFF;",
          uiOutput("mplotModuleSeriesSIZE")
         ),
         fluidRow(conditionalPanel(condition = "output.mplotModuleSeries != null",
         column(1,downloadButton(class="btn-warning",'buttonPNGmplotModuleSeries', 'HiRes PNG')),
         column(11,p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Probe(s) Mapping To Module Genes Are Summarised"))))
       ),
         conditionalPanel(condition = "output.mdatatableModuleSeries != null",
          downloadButton(class="btn-outline-primary",'mbuttonSaveTableModulesSeries', 'Table')), hr(), 
         dataTableOutput('mdatatableModuleSeries')
     )
   )
  ), #explore by module
  ###########   Lookup  ##########
tabPanel('Lookup',
  navbarPage(span(style = 'color: #000000;','Lookup'), id = 'navLookup',
     #################### Gene Lookup ###################
     tabPanel('Gene Lookup',
              wellPanel(style = "background-color: #feffee;",
                        h4(style = "margin-top: 0px;","Enter a gene name or partial name and click Lookup"),
                        h5("Use commas to separate multiple genes. Alternatively, leave box empty and click Lookup to return all genes, then use search boxes above/below table to search"),
                        fluidRow(
                          column(8,textInput('textInputGeneLookup',NULL)),
                          column(4,div(actionButton("buttonGeneLookup", "Lookup",class = "btn-success"),actionButton("buttonGeneLookupNone", "Clear")))
                        )),
              conditionalPanel(condition = "output.datatableGeneLookup != null",
                               downloadButton(class="btn-outline-primary",'buttonSaveTableGeneLookup', 'Table')), hr(), 
              dataTableOutput('datatableGeneLookup')
     ),
     #################### Module Lookup #######################
     tabPanel('Module Lookup',
              wellPanel(style = "background-color: #feffee;",
                        h4(style = "margin-top: 0px;","Enter a module name or partial name and click Lookup"),
                        h5("Use commas to separate multiple modules. Alternatively, leave box empty and click Lookup to return all modules, then use search boxes above/below table to search."),
                        fluidRow(
                          column(5,textInput('mtextInputModLookup',NULL)),
                          column(4,div(actionButton("mbuttonModLookup", "Lookup",class = "btn-success"),
                                       actionButton("mbuttonModLookupNone", "Clear"))),
                          column(3,awesomeRadio(status = 'success', 'radioArrangeModuleLookupBy','Arrange By',choices = c('Module','Title','Category'),inline = TRUE))
                        ),
                        h5("Only module names are searched, not titles or categories.")
                        ),
              conditionalPanel(condition = "output.mdatatableModuleLookup != null",
                               downloadButton(class="btn-outline-primary",'mbuttonSaveTableModuleLookup', 'Table')), hr(), 
              dataTableOutput('mdatatableModuleLookup')
     )
  )           
),
###########   Cytokines  ##########
tabPanel('Cytokines',
         navbarPage(span(style = 'color: #000000;','Cytokines'), id = 'navCytokines',
                    tabPanel('Plot',
                             wellPanel(style = "background-color: #FFFFFF;",
                                       fluidRow(
                                         column(1,style = "margin-top: 20px;",
                                                conditionalPanel(condition = "input.cselectCytokines != null && input.cselectTreatments != null && input.cselectDays != null",
                                                                 actionButton('buttonPlotCytokines','Plot',class = "btn-primary btn-block")),
                                                conditionalPanel(condition = "input.cselectCytokines == null || input.cselectTreatments == null || input.cselectDays == null", p(style = "color: #728f17; text-align: center;","Choose Variables To Plot"))
                                         ), 
                                         column(4,selectInput("cselectCytokines", "Cytokines", choices = character(0), multiple = TRUE),div(actionButton('cbuttonAddAllCytokines','All', class="btn-outline-primary"),actionButton('cbuttonAddNoneCytokines','None'))
                                                ),
                                         column(4,selectInput("cselectTreatments", "Vaccines", choices = character(0), multiple = TRUE),div(actionButton('cbuttonAddAllCytokineTreats','All', class="btn-outline-primary"),actionButton('cbuttonAddNoneCytokineTreats','None'))
                                                ),
                                         column(3,selectInput("cselectDays", "Days", choices = character(0), multiple = TRUE),div(actionButton('cbuttonAddAllCytokineDays','All', class="btn-outline-primary"),actionButton('cbuttonAddNoneCytokineDays','None'))
                                                )
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(2,prettyRadioButtons(status = 'success', 'cradioCytoMeansRaw',NULL, outline = TRUE,choiceValues = c('Fold Increase',"Concentration"),choiceNames = c('FI',"Conc"),inline = TRUE),bsTooltip("cradioCytoMeansRaw", "Which data to plot: actual concentration values or fold-increase from day 0")),
                                         column(3,prettyRadioButtons(status = 'warning', 'cradioCytokinesPlotType',NULL, outline = TRUE,choices = c("Lines",'Boxplot','Violin'),inline = TRUE)),
                                         column(4,prettyRadioButtons(status = 'danger', 'cradioCytokinesTransformY',NULL, outline = TRUE,
                                                                     choiceNames = c("Y",'ln(Y)','log10(Y)','ln1p(Y)','log2(Y)'),choiceValues = c("identity",'log','log10','log1p','log2'),inline = TRUE), bsTooltip("cradioCytokinesTransformY", "Log transform raw Y values for Boxplot and Violin. For Lines the 'Means' checkbox selects transform means or raw Y values")),
                                        column(1,style = "margin-top: 10px;",
                                               conditionalPanel(condition = "input.cradioCytokinesPlotType == 'Lines' && input.cradioCytokinesTransformY != 'identity'",
                                               awesomeCheckbox(status = 'danger', 'ccheckboxLogMeans', 'Means', value = TRUE))),
                                         column(2,style = "margin-top: 10px;",awesomeCheckbox(status = 'success', 'ccheckboxOmit0', 'Exclude Zero Y values', value = FALSE))
                                       ),
                                       fluidRow(
                                         column(1,style = "margin-top: 10px;",conditionalPanel(condition = "input.cradioCytoMeansRaw == 'Fold Increase'",
                                                                   awesomeCheckbox(status = 'success', 'ccheckboxShow1', '• |----', value = TRUE))),
                                         column(1,style = "margin-top: 10px;",
                                                conditionalPanel(condition = "input.cradioCytokinesPlotType == 'Lines'",
                                                                 awesomeCheckbox(status = 'warning', 'ccheckboxShowPoints', 'Points', value = TRUE)),
                                                conditionalPanel(condition = "input.cradioCytokinesPlotType != 'Lines'",
                                                                 awesomeCheckbox(status = 'warning', 'ccheckboxZoomQuantile', 'Crop Y', value = FALSE))),
                                         column(3,conditionalPanel(condition = "input.cradioCytokinesPlotType == 'Lines' && (input.ccheckboxLogMeans == false || input.cradioCytokinesTransformY == 'identity')",
                                                                   prettyRadioButtons('cradioCytokinesErrorType', NULL, choiceValues = list('none','ribbon','errorbar'), 
                                                                                      choiceNames = list('No Error','Ribbon','Bars'), selected = 'ribbon', inline = TRUE, outline = TRUE, status = "warning"),bsTooltip("cradioCytokinesErrorType", "Plot SEM as ribbon, error bars or omitted"))),
                                         column(1,style = "margin-top: 10px;",awesomeCheckbox(status = 'success', 'ccheckboxShowN', 'Show N', value = TRUE)),
                                         column(1, style = "margin-top: 10px;",awesomeCheckbox(status = 'success', 'ccheckboxFixedY', 'Fixed Y', value = TRUE)),
                                         column(3,prettyRadioButtons('cradioCytokinesWrap', NULL, choiceValues = list('TC','CT'), choiceNames = list('Vac↓ Cyt→', 'Cyt↓ Vac→'),
                                                                     inline = TRUE, outline = TRUE, status = "success"),bsTooltip("cradioCytokinesWrap", "How to order the panels when plotting: Cytokines across and vaccines downwards, or reverse.")),
                                         column(1, style = "margin-top: 10px;",numericInput("cnumericNumPanels",NULL,value = 3, min = 1, step = 1), bsTooltip("cnumericNumPanels", "Maximum number of panels per plot row")),
                                         column(1,sliderInput("cnumberPlotCytokinesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 4000), bsTooltip("cnumberPlotCytokinesSIZEheight", "Plot height"))
                                       ),
                                       wellPanel(style = "background-color: #FFFFFF;",
                                                 uiOutput("cplotCytokinesSIZE")
                                       ),
                                       conditionalPanel(condition = "output.cplotCytokines != null", 
                                                        downloadButton(class="btn-warning",'cbuttonPNGplotCytokines', 'HiRes PNG'))
                             ),
                             conditionalPanel(condition = "output.cdatatableCytokines != null",
                                              downloadButton(class="btn-outline-primary",'buttonSaveTableCytokines', 'Table')), hr(), 
                             dataTableOutput('cdatatableCytokines')
                    )
         )
),
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