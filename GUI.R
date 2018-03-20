# Define UI  
ui <- 
  navbarPage(span(style = 'color: #fefc78;','tmodExplorer'), id = 'navbarTop', position = "fixed-top", theme = "theme.css", windowTitle = 'tmodExplorer',
             inverse = TRUE,
             header = tagList(tags$style(type="text/css", "body {padding-top: 70px;}")),
  ######################  TABS  #########
  
  #################### Password ################
  tabPanel('Password',
           h4(style = "text-align: center;",'Please enter the password you have been given to access tmodExplorer'),
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
  tabPanel('Load data',
    h3(style = "text-align: center;","Select pre-loaded dataset to analyse and click 'Load'"),
    fluidRow(
    column(6,
        wellPanel(style = "background-color: #feffee;",
          h4("Fold Increasess From Baseline"),
          fluidRow(
            column(10,selectInput('selectDataFI', NULL, character(0))),
            column(2,actionButton('buttonLoadDataFI',label = 'Load',class = "btn-success"))
      ))),
    column(6,
      wellPanel(style = "background-color: #feffee;",
        h4("Raw Expression Values"),
        fluidRow(
        column(10,selectInput('selectDataRAW', NULL, character(0))),
        column(2,actionButton('buttonLoadDataRAW',label = 'Load',class = "btn-success"))
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
           tabPanel('Select Probes',
                    wellPanel(style = "background-color: #FFFFFF;",
                      # wellPanel(
                        h4("Select a treatment - time column to sort probe values and display responses"),
                        fluidRow(
                          column(2,selectInput('selectColumn', NULL, character(0), selectize = FALSE)),
                          column(2,awesomeCheckbox(status = 'success', 'checkboxDescending', 'Sort Descending', value = TRUE)),
                          column(2,awesomeCheckbox(status = 'success', 'checkboxProbesGenes', 'Gene Averages', value = FALSE))
                        ),
                      # ),
                      conditionalPanel(condition = "input.selectColumn != null",
                      h4(align = 'center','Filter probes by a combination of options below. Selected filters are applied in order left to right'),
                      fluidRow(
                          column(4,
                            wellPanel(style = "background-color: #feffee;",
                              h4(awesomeCheckbox(status = 'success', 'checkboxSelectKeyword', '1. Using regex', value = FALSE)),
                              textInput('textInputKeyword',NULL),
                              h4("Search:"),
                              awesomeRadio(status = 'success', 'radioKeywordColumn',NULL,choices = c('Description','Gene','Probe'), inline = TRUE),
                              conditionalPanel(condition = "input.radioKeywordColumn != 'Description'",p(style = "color: #cfdaa2;","Spaces are stripped from Gene and Probe names")),
                              conditionalPanel(condition = "input.radioKeywordColumn == 'Description'",p(style = "color: #cfdaa2;","Spaces are kept in search for Description"))
                          )),
                          column(8,
                            fluidRow(
                              column(6,
                                wellPanel(style = "background-color: #feffee;",
                                  h4(awesomeCheckbox(status = 'success', 'checkboxSelectValues', '2. Sorted Column Values Within Range:', value = FALSE)),
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
                                  h4(awesomeCheckbox(status = 'success', 'checkboxSelectRows', '3. Sorted Column Row Numbers', value = TRUE)),
                                  fluidRow(
                                    column(6,numericInput("numberGenesStart", "From Row:", 0, min = 0, max = NA, step = 5)), 
                                    column(6,numericInput("numberGenesEnd", "To Row:", 10, min = 0, max = NA, step = 5))
                                  ),
                                  p(style = "color: #cfdaa2;", "More than 100 rows will result in slow response")
                                )
                              )
                            )
                          )
                        ),
                      fluidRow(column(4),column(4,actionButton('buttonApplySelection','Apply Selections',class = "btn-success btn-block")),column(4))
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
                          actionButton('buttonPlotSeries','Plot',class = "btn-success btn-block")),
                        conditionalPanel(condition = "input.selectColumnsForSeries == null", p(style = "color: #728f17; text-align: center;","Choose Some Columns To Plot")),
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
                              awesomeCheckbox(status = 'success', 'checkboxShowZeroSeries', 'Zero', value = TRUE),
                              sliderInput("numberPlotTopGenesSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500)
                            ))
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
                uiOutput("plotTopGenesSeriesSIZE")),
               conditionalPanel(condition = "output.plotTopGenesSeries != null",
                                downloadButton(class="btn-warning",'buttonPNGplotTopGenesSeries', 'Plot As PNG'))
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
                 column(1,awesomeCheckbox(status = 'success', 'checkboxShowLegendGenesModules', 'Legend', value = FALSE)),
                 column(1,awesomeCheckbox(status = 'success', 'checkboxShowZeroGenesModules', 'Zero', value = TRUE)),
                 column(3,awesomeRadio(status = 'success', 'radioGroupProbeModulesBy','Group By',choices = c('Module','Title'),inline = TRUE)),
                 column(1,awesomeCheckbox(status = 'success', 'checkboxGGplotGenesModules', 'ggplot2', value = FALSE)),
                 column(2,sliderInput("numberPlotGenesModulesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500)),
                 column(4,awesomeCheckbox(status = 'success', 'checkboxShowPsuedoModuleGenesModules', 'Include Selected As Module', value = TRUE))
               ),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotGenesModulesSIZE")
               ),
               downloadButton(class="btn-warning",'buttonPNGplotGenesModules', 'Plot As PNG')
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
                 column(2,awesomeCheckbox(status = 'success', 'checkboxShowLegendModuleGenes', 'Legend', value = FALSE)),
                 column(2,awesomeCheckbox(status = 'success', 'checkboxShowZeroModuleGenes', 'Zero', value = TRUE)),
                 column(2,awesomeCheckbox(status = 'success', 'checkboxGGplotModuleGenes', 'ggplot2', value = FALSE)),
                 column(2,sliderInput("numberPlotModuleGenesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500))               ))),
               wellPanel(style = "background-color: #FFFFFF;",
                uiOutput("plotModuleGenesSIZE")),
                downloadButton(class="btn-warning",'buttonPNGplotModuleGenes', 'Plot As PNG')
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
                                 awesomeCheckbox(status = 'success', 'checkboxShowZeroModuleSeries', 'Zero', value = TRUE),
                                 sliderInput("numberPlotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500)
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
             conditionalPanel(condition = "output.plotModuleSeries != null",
              downloadButton(class="btn-warning",'buttonPNGplotModuleSeries', 'Plot As PNG'))
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
    h4(align = 'center', textOutput('textDataNameMods')),
    navbarPage(span(style = 'color: #000000;','Module'), id = 'navModule', header = h4(align = 'center', textOutput('textFiltersMods')),
      #################### Selecting Modules ################
      tabPanel('Select Modules',
      wellPanel(style = "background-color: #FFFFFF;",
        # wellPanel(
          h4("Select a treatment - time column to sort module values and display responses"),
          fluidRow(
            column(2, selectInput('mselectColumn', NULL, character(0), width = 400, selectize = F)),
            column(2,awesomeCheckbox(status = 'success', 'mcheckboxDescending', 'Sort Descending', value = TRUE)),
            column(2,awesomeCheckbox(status = 'success', 'mcheckboxModuleMedians', 'Use Medians Not Means', value = FALSE))
          ),
        # ),
        conditionalPanel(condition = "input.mselectColumn != null",
        h4(align = 'center','Filter modules by a combination of options below. Selected filters are applied in order left to right'),
        fluidRow(
          column(4,
          wellPanel(style = "background-color: #feffee;",
            h4(awesomeCheckbox(status = 'success', 'mcheckboxSelectKeyword','1. Using regex', value = FALSE)),
            textInput('mtextInputKeyword',NULL),
            h4("Search:"),
            awesomeRadio(status = 'success', 'mradioKeywordColumn',NULL,choices = c('Title','Module'), inline = TRUE),
            conditionalPanel(condition = "input.mradioKeywordColumn == 'Module'",p(style = "color: #cfdaa2;","Spaces are stripped from Module names")),
            conditionalPanel(condition = "input.mradioKeywordColumn == 'Title'",p(style = "color: #cfdaa2;","Spaces are kept in search for Title"))
          )),
          column(8,
           fluidRow(
            column(6,
            wellPanel(style = "background-color: #feffee;",
              h4(awesomeCheckbox(status = 'success', 'mcheckboxSelectValues', '2. Sorted Column Values Within Range:', value = FALSE)),
            fluidRow(
              column(6,numericInput("mnumberExpressionMin", "Lowest:", value = 0)),
              column(6,numericInput("mnumberExpressionMax", "Highest:", value = 0))
            ),
            actionButton('mbuttonResetValuesRangeCol','Column', class = 'btn-outline-primary'),
            actionButton('mbuttonResetValuesRangeData','Data', class = 'btn-outline-primary')
          )),
          column(6,
            wellPanel(style = "background-color: #feffee;",
              h4(awesomeCheckbox(status = 'success', 'mcheckboxSelectRows', '3. Sorted Column Row Numbers', value = TRUE)),
              fluidRow(
                column(6,numericInput("mnumberModsStart", "From Row:", 0, min = 0, max = NA, step = 5)),
                column(6,numericInput("mnumberModsEnd", "To Row:", 10, min = 0, max = NA, step = 5))
              ),
              p(style = "color: #cfdaa2;", "More than 100 modules will result in slow response")
            ))
          )
        )# right column wells
        ),
        fluidRow(column(4),column(4,actionButton('mbuttonApplySelection','Apply Selections',class = "btn-success btn-block")),column(4))
        )
      )
     ),
    #################### Top Modules #######################
     tabPanel('Selected Modules',
       wellPanel(style = "background-color: #FFFFFF;",
         h4('Expression Values Of Selected Modules'),
         fluidRow(
           column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowLegendGenesModules', 'Legend', value = FALSE)),
           column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowZeroGenesModules', 'Zero', value = TRUE)),
           column(3,awesomeRadio(status = 'success', 'mradioGroupTitleName','Group By',choices = c('Module','Title'),inline = TRUE)),
           column(3,awesomeCheckbox(status = 'success', 'mcheckboxGGplotGenesModules', 'ggplot2', value = FALSE)),
           column(2,sliderInput("numbermplotSelectedModulesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500))         ),
         wellPanel(style = "background-color: #FFFFFF;",
                   uiOutput("mplotSelectedModulesSIZE")
          ),
         downloadButton(class="btn-warning",'buttonPNGmplotSelectedModules', 'Plot As PNG')
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
                    awesomeCheckbox(status = 'success', 'mcheckboxShowZeroModuleSeries', 'Zero', value = TRUE),
                    awesomeCheckbox(status = 'success', 'mcheckboxShowFacetModuleSeries', 'Split', value = TRUE),
                    awesomeRadio(status = 'success', 'mradioGroupTitleNameModuleSeries','Group By:',choices = c('Title','Module'), selected = 'Module'),
                    sliderInput("numbermplotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500)
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
                    h4("Select Modules To Plot From One Of The Options Below:"),
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
         conditionalPanel(condition = "output.mplotModuleSeries != null",
          downloadButton(class="btn-warning",'buttonPNGmplotModuleSeries', 'Plot As PNG'))
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
                        h4("Enter a gene name or partial name and click Lookup"),
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
                        h5("Enter a module name and click Lookup"),
                        h6("Use commas to separate multiple modules. Alternatively, leave box empty and click Lookup to return all modules, then use search boxes above/below table to search"),
                        fluidRow(
                          column(5,textInput('mtextInputModLookup',NULL)),
                          column(4,div(actionButton("mbuttonModLookup", "Lookup",class = "btn-success"),
                                       actionButton("mbuttonModLookupNone", "Clear"))),
                          column(3,awesomeRadio(status = 'success', 'radioArrangeModuleLookupBy','Arrange By',choices = c('Module','Title','Category'),inline = TRUE))
                        )),
              conditionalPanel(condition = "output.mdatatableModuleLookup != null",
                               downloadButton(class="btn-outline-primary",'mbuttonSaveTableModuleLookup', 'Table')), hr(), 
              dataTableOutput('mdatatableModuleLookup')
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
