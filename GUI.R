# Define UI  
ui <-
  tagList(
    useShinyjs(),  # Set up shinyjs
    tags$style(".fa-info-circle {color:#fefc78};"),
    hidden(div(id = "hiddenDiv",
    navbarPage(span(style = 'color: #fefc78;','tmodExplorer'), id = 'navbarTop', position = "static-top", theme = "theme.css", windowTitle = 'tmodExplorer',
               inverse = TRUE,
               header = tagList(tags$style(type="text/css", "body {padding-top: 0px;};")),
               
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
     #   #################### Dataset Load ######################
     tabPanel('Load transcriptomics',
              h3(style = "text-align: center;","Select a transcriptomics dataset to analyse"),
              fluidRow(
                column(10, offset = 1,
                       wellPanel(style = "background-color: #feffee;",
                                 fluidRow(
                                   column(8,pickerInput(inputId = 'selectDataFI', choices = NULL, options = list(`style` = "btn-success"))),
                                   column(4,
                                          actionBttn('buttonLoadDataFI','Load Dataset',style = 'unite', size = 'sm', color = 'success', block = TRUE)
                                   )
                                 ))
                )
              ),
              hidden(h4(id = "textDataNameHeader", style = "text-align: center; margin-top: 0px; margin-bottom:5px;  margin-left: 0px; margin-right: 0px; background-color: #b59800; color: #FFFFFF;padding-top: 10px; padding-bottom: 10px;",textOutput('textDataName'))),
              conditionalPanel(condition = "output.datatableAll != null",downloadButton(class="btn-outline-primary",'buttonsavedatatableAll', 'Table')),
              hr(),
              dataTableOutput('datatableAll')
     ),
     # ############## PROBES #################
     tabPanel('Explore By Gene',
              hidden(h4(id = "textDataNameProbesHeader", h4(style = "text-align: center; margin-top: 0px; margin-bottom:0px;  margin-left: 0px; margin-right: 0px; background-color: #b59800; color: #FFFFFF;padding-top: 10px; padding-bottom: 10px;", textOutput('textDataNameProbes')))),
              navbarPage(span(style = 'color: #000000;','Gene'), id = 'navProbe',
                         header = hidden(tagList(div(id = "navProbeHeader", h4(style = "text-align: center; margin-top: 0px; margin-bottom:5px;  margin-left: 0px; margin-right: 0px; color: #FFFFFF; background-color: #ae6500;padding-top: 10px; padding-bottom: 10px;", textOutput('textFiltersProbes'))))),
                         #################### Selecting  ################
               tabPanel('Select Genes',
                        conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",
                                         fluidRow(style = "margin-bottom:10px;",
                                           column(4, offset = 4, style = "margin-top: 4px; color: black ", 
                                                  actionBttn('buttonApplySelection','Apply Filters',style = 'unite', size = 'sm', color = 'warning', block = TRUE),
                                                  bsTooltip('buttonApplySelection',"Apply filters (in order 1-2-3 left → right) to select spots or genes for plotting", placement = "top")),
                                           column(2, offset = 2,
                                                  numericInput("rowsLimitNumeric", NULL, value = 100, min = 50, step = 50),
                                                  bsTooltip("rowsLimitNumeric", "Limit to number of Spots / Genes returned. Suggest set to ~ 100 to avoid a very slow response", placement = 'top')
                                                  )
                                         )),
                        fluidRow(
                          column(2,# keywrd column
                            wellPanel(style = "background-color: #feffee;",
                                conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",
                                awesomeCheckbox(status = 'success', 'checkboxSelectKeyword', label = h4(style = "margin-top: 0px; color: #728f17;font-weight: bold;",'1. Keyword regex search'), value = FALSE),
                                conditionalPanel(condition = "input.checkboxSelectKeyword == true",
                                conditionalPanel(condition = "input.selectKeywordColumn != 'Description'",p(style = "color: #44b84b;","Spaces will be stripped")),
                                conditionalPanel(condition = "input.selectKeywordColumn == 'Description'",p(style = "color: #44b84b;","Spaces will be kept")),
                                textInput('textInputKeyword',NULL),
                                h4(style = "margin-top: 0px;","Search:"),
                                pickerInput('selectKeywordColumn',label = NULL,choices = c('Gene','Spot','ProbeName','SystematicName','Description'), options = list(`style` = "btn-success")),
                                awesomeCheckbox("checkboxGeneSearchWholeWord","Whole Word", TRUE,status = "danger")
                            )))),
                          column(10, # other searches
                                 wellPanel(style = "background-color: #ffffff;",
                                           fluidRow( # vacc - day pickers
                                             column(3,pickerInput('selectColumnVaccine', choices = NULL, options = list(`style` = "btn-success"))),
                                             column(2,pickerInput('selectColumnDay', choices = NULL, options = list(`style` = "btn-success"))),
                                             column(7,
                                              fluidRow(
                                                fluidRow(
                                                  column(4, awesomeCheckbox("checkboxRowsAnyDay",label = h4(style = "margin-top: 0px;font-weight: bold;",'All Times'), value = FALSE,status = 'success'),
                                                         bsTooltip("checkboxRowsAnyDay","Sort rows by value for selected Treatment on all days")),
                                                  column(4, awesomeCheckbox(status = 'danger', 'checkboxProbesGenes', label = h4(style = "margin-top: 0px; color: #b90600;font-weight: bold;",'Gene Averages'), value = FALSE)),
                                                  column(4,awesomeCheckbox(status = 'success', 'checkboxDescending', label = h4(style = "margin-top: 0px;font-weight: bold;",'Sort Descending'), value = TRUE))
                                                  )))
                                           ),# vacc day
                                           ### searches 1 & 2
                                               fluidRow(
                                                column(10,
                                                      conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",
                                                      wellPanel(style = "background-color: #feffee;",
                                                        fluidRow(
                                                          column(4,
                                                            awesomeCheckbox(status = 'success', 'checkboxSelectValues', label = h4(style = "margin-top: 0px; margin-bottom: 0px; color: #728f17;font-weight: bold;",'2. Values In Range:'), value = FALSE)
                                                          ),
                                                          conditionalPanel(condition = "input.checkboxSelectValues == true",
                                                          column(8,radioGroupButtons('radioFilterByRowKinetics', NULL,
                                                                   choiceValues = list('row', 'kinetics'),choiceNames = list('Treat~Time', 'Treat~Kinetics'),
                                                                   individual = FALSE, justified = TRUE, status = "primary")
                                                          ))
                                                        ),
                                                        conditionalPanel(condition = "input.checkboxSelectValues == true",
                                                        conditionalPanel(condition = "input.radioFilterByRowKinetics == 'row'",
                                                        fluidRow(
                                                            column(6,numericInput("numberExpressionMin", "Lowest:", value = 0)),
                                                            column(6,numericInput("numberExpressionMax", "Highest:", value = 0))
                                                        ),
                                                         fluidRow(
                                                            column(6,
                                                              actionButton('buttonResetValuesRangeCol',NULL,icon = icon("tag"), class = 'btn-outline-primary'),
                                                              actionButton('buttonResetValuesRangeData',NULL, icon = icon("folder-open"), class = 'btn-outline-primary'))
                                                         )
                                                        ),
                                                        conditionalPanel(condition = "input.radioFilterByRowKinetics == 'kinetics'",
                                                        fluidRow(
                                                          column(5,
                                                            conditionalPanel(condition = "input.checkboxRowsAnyDay == true",
                                                            p(style = "margin-top: 0px; margin-bottom: 0px; text-align: center; color: #b90600; font-size: 0.9em","All Times checkbox will be ignored")),
                                                            plotOutput("plotShapeMiniplot", height = "150px", click = "click_plotShapeMiniplot", dblclick = "dblclick_plotShapeMiniplot"),
                                                            bsTooltip("plotShapeMiniplot","Click * to select day, double-click to toggle Ignore", placement = 'top')
                                                          ),
                                                          column(7, 
                                                            fluidRow(
                                                              column(2, actionButton('buttonShapeSaveDay','Set', class = 'btn-warning')),
                                                              column(5,pickerInput('selectShapeDay', choices = NULL, options = list(`style` = "btn-success"))),
                                                               column(5,awesomeCheckbox(status = 'success', 'checkboxShapeSkipDay', label = "Ignore", value = TRUE))
                                                            ),
                                                            fluidRow(  
                                                              conditionalPanel(condition = "input.checkboxShapeSkipDay == false",
                                                                               column(6,numericInput("numberShapeDayMin", "Lowest:", value = 0)),
                                                                               column(6,numericInput("numberShapeDayMax", "Highest:", value = 0)))
                                                            ),
                                                            fluidRow(
                                                              column(6, 
                                                                     actionButton('buttonCopyValuesShapeData',NULL, icon = icon("copy"), class = 'btn-warning'),
                                                                     actionButton('buttonPasteValuesShapeData',NULL, icon = icon("paste"), class = 'btn-warning')),
                                                              conditionalPanel(condition = "input.checkboxShapeSkipDay == false",
                                                              column(6, 
                                                                actionButton('buttonResetValuesShapeVaccine',NULL,icon = icon("tag"), class = 'btn-outline-primary'),
                                                                actionButton('buttonResetValuesShapeData',NULL, icon = icon("folder-open"),class = 'btn-outline-primary')))
                                                            )
                                                          )),
                                                        fluidRow(style = "margin-top: 10px;",
                                                          column(2, actionBttn('buttonResetKineticsData',"Reset",style = 'stretch', icon = icon("folder-open"), color = "warning", size = 'sm')),bsTooltip("buttonResetKineticsData","Reset To Dataset Max Min"),
                                                          column(2, actionBttn('buttonResetKineticsTreat',"Reset",style = 'stretch',  icon = icon("tag"), color = "warning", size = 'sm')),bsTooltip("buttonResetKineticsTreat","Reset To Treatment Max Min"),
                                                          column(2,downloadButton(class="btn-outline-primary",'buttonSaveShapeKinetics', "Export")),
                                                          column(6,fileInput('buttonLoadShapeKinetics', label = NULL, buttonLabel = "Import…", accept = c(".rds")))
                                                        )
                                                      )
                                                 ))) # well conpan pickers
                                                 ),
                                                 column(2,
                                                  conditionalPanel(condition = "input.selectColumnDay != null && input.selectColumnVaccine != null",
                                                   wellPanel(style = "background-color: #feffee;",
                                                             awesomeCheckbox(status = 'success', 'checkboxSelectRows', label = h4(style = "margin-top: 0px; color: #728f17;font-weight: bold;",'3. Rows In Range:'), value = TRUE),
                                                             conditionalPanel(condition = "input.checkboxSelectRows == true",
                                                              numericInput("numberGenesStart", "From:", 0, min = 0, max = NA, step = 5),
                                                              numericInput("numberGenesEnd", "To:", 10, min = 0, max = NA, step = 5)
                                                   )))
                                                 )
                                               ) # Searches 1 & 2

                                 )
                          )#column
                        )# row
               ),
                         #################### Top Genes #######################
                         tabPanel('Selected Genes',
                                  
                                  h4(style = "margin-top: 0px;",'Spots Or Genes Meeting The Filters, Sorted By Values In Selected Treatment-Time Combination'),
                                  fluidRow(
                                    column(4,downloadButton(class="btn-outline-primary",'buttonSaveTableProbes', 'Table'),
                                    downloadButton(class="btn-warning",'buttonSaveTableTopGenesUpPlot', 'Table As PNG')),
                                    column(2,pickerInput('pickerSaveListTopGenes',label = NULL, width = '100%', choices = c('Gene','Spot','ProbeName','SystematicName','Description'), options = list(`style` = "btn-danger"))),
                                    column(2,downloadButton(class="btn-danger",'buttonSaveListGenes', 'List'), bsTooltip("buttonSaveListGenes", "Download List To Paste Into Regex Keyword Search"))
                                  ),
                                  hr(),
                                  dataTableOutput('datatableTopGenesUp')
                         ),
                         #################### Top Spots Series ################
                         tabPanel('Genes:Series',
                                  wellPanel(style = "background-color: #FFFFFF",
                                            h4(style = "margin-top: 0px;",'Time Course Of Spots Or Genes Meeting The Filters, By Treatment~Time'),
                                            fluidRow(
                                              column(4,
                                                     wellPanel(style = "background-color: #feffee;",
                                                               conditionalPanel(condition = "input.selectVaccinesForSeries != null && input.selectDaysForSeries != null && input.selectGenesProbesForSeries != null",
                                                                                actionBttn('buttonPlotSeries','Plot',style = 'unite', size = 'sm', color = 'warning', block = TRUE)
                                                                                ),
                                                               conditionalPanel(condition = "input.selectVaccinesForSeries == null || input.selectDaysForSeries == null || input.selectGenesProbesForSeries == null",
                                                                                p(style = "color: #728f17; text-align: center;","Choose Some Variables To Plot")),
                                                               fluidRow(
                                                                 column(6,
                                                                        awesomeRadio(status = 'warning', 'radioBoxLineProbesSeries', " ", choices = c(Lines = 'Lines',Boxplot = 'Boxplot')),
                                                                        conditionalPanel(condition = "input.radioBoxLineProbesSeries != 'Boxplot'",
                                                                                         span(id = "spancheckboxShowProbesOfGenesSeries",style = "color: red; font-weight: bold;  font-family: Verdana;",
                                                                                              awesomeCheckbox(status = 'danger', 'checkboxShowProbesOfGenesSeries', 'Gene⫷Spots', value = FALSE),
                                                                                              bsTooltip("spancheckboxShowProbesOfGenesSeries", "When Selected The Value Of Individual Spots For Each Gene Will Be Plotted Instead Of Gene Averages")),
                                                                                         awesomeCheckbox(status = 'success', 'checkboxShowPointsSeries', 'Points', value = FALSE),
                                                                                         conditionalPanel(condition = "input.checkboxShowProbesOfGenesSeries != true", awesomeCheckbox(status = 'success', 'checkboxShowSEMSeries', 'SEM', value = TRUE)),
                                                                                         conditionalPanel(condition = "input.radioFilterByRowKinetics != 'row' && input.checkboxSplitSeries == true", style = "color: #4178b6;",
                                                                                          awesomeCheckbox(status = 'primary', 'checkboxShowKineticsSeries', 'Kinetics Filters', value = TRUE)),
                                                                                         bsTooltip("numericNumPanelsTopGenesSeries", "Maximum panels per row when Split"),
                                                                                         numericInput("numericNumPanelsTopGenesSeries",NULL,value = 3, min = 1, step = 1)
                                                                        )
                                                                 ),
                                                                 column(6,
                                                                        awesomeCheckbox(status = 'success', 'checkboxSplitSeries', 'Split', value = TRUE),
                                                                        conditionalPanel(condition = "input.checkboxSplitSeries == true && input.radioBoxLineProbesSeries != 'Boxplot'",
                                                                          awesomeCheckbox(status = 'success', 'checkboxShowGridSeries', 'Gridlines', value = TRUE)),
                                                                        awesomeCheckbox(status = 'success', 'checkboxShowZeroSeries', '0 |----', value = TRUE),
                                                                        awesomeCheckbox(status = 'success', 'checkboxShowLegendSeries', 'Legend', value = FALSE),
                                                                        sliderInput("numberPlotTopGenesSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotTopGenesSeriesSIZEheight", "Plot height")
                                                                 ))
                                                     )),
                                              column(8,
                                                     wellPanel(style = "background-color: #feffee;",
                                                               fluidRow(
                                                                 column(4,selectInput('selectVaccinesForSeries', label = "Treatment", choices = character(0), multiple = TRUE)),
                                                                 column(2,div(style = "margin-top: 20px;",actionButton('buttonAddAllVaccinesSeries','All', class="btn-outline-primary"),actionButton('buttonRemoveAllVaccinesSeries','None'))),
                                                                 column(4,selectInput('selectDaysForSeries', label = "Times", choices = character(0), multiple = TRUE)),
                                                                 column(2,div(style = "margin-top: 20px;",actionButton('buttonAddAllDaysSeries','All', class="btn-outline-primary"),actionButton('buttonRemoveAllDaysSeries','None')))
                                                               )
                                                     ),
                                                     wellPanel(
                                                       style = "background-color: #feffee;",
                                                       fluidRow(
                                                         column(9,selectInput('selectGenesProbesForSeries', label = "Spots / Genes", choices = character(0), multiple = TRUE)),
                                                         column(3,div(style = "margin-top: 20px;",
                                                                      actionButton('buttonAddAllGenesProbesSeries','All', class="btn-outline-primary"),
                                                                      actionButton('buttonRemoveGenesProbesSeries','None')))
                                                       )
                                                     )
                                              )
                                            ),
                                            wellPanel(style = "background-color: #FFFFFF;",
                                                      uiOutput("plotTopGenesSeriesSIZE")),
                                            conditionalPanel(condition = "output.plotTopGenesSeries != null",
                                             fluidRow(
                                               column(1, downloadButton(class="btn-warning",'buttonPNGplotTopGenesSeries', 'HiRes PNG')),
                                               column(8, offset = 1,
                                                      conditionalPanel(condition = "input.radioBoxLineProbesSeries == 'Lines' && output.plotTopGenesSeriesBRUSH == null",p(style = "text-align: center; color:#b1cd46;","Click points to identify, drag to select")),
                                                      tableOutput("plotTopGenesSeriesBRUSH")
                                                      ),
                                               column(2,textOutput("plotTopGenesSeriesGENEMOD"),p(), span(style = "color: #008f51",textOutput("plotTopGenesSeriesSPOT")))
                                             ))
                                  ),
                                  conditionalPanel(condition = "output.datatableTopGenesSeries != null",
                                                   downloadButton(class="btn-outline-primary", 'buttonSaveTableProbesSeries', 'Table')), hr(),
                                  dataTableOutput('datatableTopGenesSeries')),
                         #################### Genes->Modules ##################
                         tabPanel('Genes->Modules',
                                  
                                  h4(style = "margin-top: 0px;",'Modules Associated With Selected Spots or Genes'),
                                  div(downloadButton(class="btn-outline-primary",'buttonSaveTableGenesModules', 'Table'),
                                      downloadButton(class="btn-warning",'buttonSaveTableGenesModulesPlot', 'Table As PNG')),
                                  hr(),
                                  dataTableOutput('datatableGenesModules')),
                         #################### Modules #########################
                         tabPanel('Modules',
                                  wellPanel(style = "background-color: #FFFFFF",
                                            h4(style = "margin-top: 0px;",'Expression Values Of Modules Associated With Selected Spots / Genes'),
                                            fluidRow(
                                              column(3,awesomeCheckbox(status = 'success', 'checkboxShowPsuedoModuleGenesModules', 'Include Selected As Module', value = TRUE)),
                                              column(1,awesomeCheckbox(status = 'success', 'checkboxShowLegendGenesModules', 'Legend', value = FALSE)),
                                              column(1,awesomeCheckbox(status = 'success', 'checkboxShowZeroGenesModules', '0 |----', value = TRUE)),
                                              column(3,style = "margin-top: 10px;",
                                                     awesomeRadio(status = 'success', 'radioGroupProbeModulesBy',NULL,choices = c('Module','Title'),inline = TRUE),
                                                     bsTooltip("radioGroupProbeModulesBy", "Group by Modules or Module Titles (may be fewer)")),
                                              column(1,awesomeCheckbox(status = 'success', 'checkboxGGplotGenesModules', 'ggplot2', value = FALSE)),
                                              column(2, offset = 1, style = "margin-top: -10px;", sliderInput("numberPlotGenesModulesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotGenesModulesSIZEheight", "Plot height"))
                                            ),
                                            wellPanel(style = "background-color: #FFFFFF;",
                                                      uiOutput("plotGenesModulesSIZE")
                                            ),
                                            fluidRow(
                                              column(1,downloadButton(class="btn-warning",'buttonPNGplotGenesModules', 'HiRes PNG')),
                                              column(11, p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Spot(s) Mapping To Module Genes Are Summarised"))
                                            )),
                                  downloadButton(class="btn-outline-primary",'buttonSaveTableModulesSummary', 'Table'),
                                  downloadButton(class="btn-outline-primary",'buttonSaveTableModulesRaw', 'Raw Dataset'),
                                  downloadButton(class="btn-warning",'buttonSaveTableModulesSummaryPlot', 'Table As PNG'),
                                  downloadButton(class="btn-warning",'buttonSaveTableModulesSummaryListPlot', 'Modules List As PNG'),
                                  downloadButton(class="btn-danger",'buttonSaveTableModulesSummaryList', 'Modules & Titles List'), bsTooltip("buttonSaveTableModulesSummaryList", "Modules & Titles List To Paste Into regex Keyword Search"),
                                  hr(),
                                  dataTableOutput('datatableSelModulesOnly')),
                         #################### Modules->Genes ###################
                         tabPanel('Module->Genes',
                                  wellPanel(style = "background-color: #FFFFFF;",
                                            h4(style = "margin-top: 0px;",'Expression Values Of Genes Within Modules Associated With Selected Spots / Genes'),
                                            fluidRow(
                                              column(5,pickerInput(inputId = 'selectModuleForGenes', label = NULL, choices = NULL, inline = TRUE,options = list(`style` = "btn-success"))),
                                              column(2, awesomeCheckbox(status = 'success', 'checkboxShowMissingModuleGenes', 'Show Missing Genes', value = TRUE)),
                                              column(1, awesomeCheckbox(status = 'success', 'checkboxShowLegendModuleGenes', 'Legend', value = FALSE)),
                                              column(1,awesomeCheckbox(status = 'success', 'checkboxShowZeroModuleGenes', '0 |----', value = TRUE)),
                                              column(1,awesomeCheckbox(status = 'success', 'checkboxGGplotModuleGenes', 'ggplot2', value = FALSE)),
                                              column(2, style = "margin-top: -10px;", sliderInput("numberPlotModuleGenesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotModuleGenesSIZEheight", "Plot height"))
                                            ),
                                            wellPanel(style = "background-color: #FFFFFF;",
                                                      uiOutput("plotModuleGenesSIZE")
                                            ),
                                            fluidRow(
                                              column(1,downloadButton(class="btn-warning",'buttonPNGplotModuleGenes', 'HiRes PNG')),
                                              column(11,p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Spot(s) Mapping To Module Genes Are Summarised"))
                                            )),
                                  downloadButton(class="btn-outline-primary",'buttonSaveTableModulesGenes', 'Table'),
                                  downloadButton(class="btn-danger",'buttonTableModulesGenesList', 'Module Genes List'), bsTooltip("buttonTableModulesGenesList", "Gene List To Paste Into regex Keyword Search"),
                                  hr(),
                                  dataTableOutput('datatableModuleGenes')),
                         
                         #################### Modules Series ###################
                         tabPanel('Modules:Series',
                                  wellPanel(style = "background-color: #FFFFFF;",
                                            h4(style = "margin-top: 0px;",'Time Course Of Modules Associated With Selected Spots / Genes'),
                                            fluidRow(
                                              column(3,
                                                     wellPanel(style = "background-color: #feffee;",
                                                               conditionalPanel(condition = "input.selectColumnForModuleSeriesVaccines != null && input.selectColumnForModuleSeriesDays != null && input.selectModuleForSeries != null",
                                                                                actionBttn('buttonPlotModuleSeries','Plot',style = 'unite', size = 'sm', color = 'warning', block = TRUE)
                                                               ),
                                                               conditionalPanel(condition = "input.selectColumnForModuleSeriesVaccines == null || input.selectColumnForModuleSeriesDays == null || input.selectModuleForSeries == null",
                                                                                p(style = "color: #728f17; text-align: center;","Choose Treatment~Times & Modules To Plot")),
                                                               fluidRow(
                                                                 column(6,
                                                                        awesomeRadio(status = 'warning', 'radioRibbonBoxModuleSeries'," ",choices = c('Lines','Boxplot')),
                                                                        awesomeCheckbox(status = 'success', 'checkboxShowFacetModuleSeries', 'Split', value = TRUE),
                                                                        conditionalPanel(condition = "input.radioRibbonBoxModuleSeries == 'Lines'",
                                                                                         conditionalPanel(condition = "input.checkboxShowFacetModuleSeries == true",
                                                                                                          awesomeCheckbox(status = 'success', 'checkboxShowGridModuleSeries', 'Gridlines', value = TRUE)),
                                                                                         awesomeCheckbox(status = 'success', 'checkboxShowPointsModuleSeries', 'Points', value = FALSE),
                                                                                         awesomeCheckbox(status = 'success', 'checkboxShowSEModuleSeries', 'SEM', value = FALSE)
                                                                        )
                                                                 ),
                                                                 column(6,
                                                                        awesomeCheckbox(status = 'success', 'checkboxShowLegendModuleSeries', 'Legend', value = FALSE),
                                                                        awesomeCheckbox(status = 'success', 'checkboxShowZeroModuleSeries', '0 |----', value = TRUE),
                                                                        bsTooltip("numericNumPanelsPlotModuleSeries", "Maximum panels per row when Split"),
                                                                        numericInput("numericNumPanelsPlotModuleSeries",NULL,value = 3, min = 1, step = 1),
                                                                        sliderInput("numberPlotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotModuleSeriesSIZEheight", "Plot height")
                                                                 )
                                                               )
                                                     )
                                              ),
                                              column(9,
                                                     wellPanel(style = "background-color: #feffee;",
                                                               fluidRow(
                                                                 column(6,
                                                                        fluidRow(
                                                                          column(8, selectInput('selectColumnForModuleSeriesVaccines', label = "Treatment", choices = character(0), multiple = TRUE)),
                                                                          column(4,div(style = "margin-top: 20px;",
                                                                                       actionButton('buttonAddAllColumnsModuleSeriesVaccines','All', class="btn-outline-primary"),
                                                                                       actionButton('buttonRemoveAllColumnsModuleSeriesVaccines','None')))
                                                                        )
                                                                 ),
                                                                 column(6,
                                                                        fluidRow(
                                                                          column(8, selectInput('selectColumnForModuleSeriesDays', label = "Times", choices = character(0), multiple = TRUE)),
                                                                          column(4, div(style = "margin-top: 20px;",
                                                                                        actionButton('buttonAddAllColumnsModuleSeriesDays','All', class="btn-outline-primary"),
                                                                                        actionButton('buttonRemoveAllColumnsModuleSeriesDays','None'))))
                                                                 )
                                                               )
                                                     ),
                                                     wellPanel(style = "background-color: #feffee;",
                                                               fluidRow(
                                                                 column(9, selectInput('selectModuleForSeries', label = 'Modules', character(0), multiple = TRUE),
                                                                        awesomeCheckbox(status = 'success', 'checkboxShowPseudoModuleModuleSeries', 'Include Selected As Module', value = TRUE)),
                                                                 column(3, div(style = "margin-top: 20px;",actionButton('buttonAddAllModulesModuleSeries','All', class="btn-outline-primary"),
                                                                               actionButton('buttonRemoveAllModulesModuleSeries','None'))
                                                                 )
                                                               ))
                                              )
                                            ),
                                            wellPanel(style = "background-color: #FFFFFF;",
                                                      uiOutput("plotModuleSeriesSIZE")
                                            ),
                                            fluidRow(conditionalPanel(condition = "output.plotModuleSeries != null",
                                                                      column(1,downloadButton(class="btn-warning",'buttonPNGplotModuleSeries', 'HiRes PNG')),
                                                                      column(8, offset = 1,
                                                                             conditionalPanel(condition = "input.radioRibbonBoxModuleSeries == 'Lines' && output.plotModuleSeriesBRUSH == null",p(style = "text-align: center; color:#b1cd46;","Click points to identify, drag to select")),
                                                                             tableOutput("plotModuleSeriesBRUSH")),
                                                                      column(2,textOutput("plotModuleSeriesGENEMOD"))
                                                                      )
                                            )
                                  ),
                                  conditionalPanel(condition = "output.datatableModuleSeries != null",
                                                   downloadButton(class="btn-outline-primary",'buttonSaveTableModulesSeries', 'Table')),
                                  hr(),
                                  dataTableOutput('datatableModuleSeries')
                         )
                         
              ) # navProbe
     ),# explore by spot
     ############## MODULES #################
     tabPanel('Explore By Module',
              hidden(h4(id = "textDataNameModsHeader", style = "text-align: center; margin-top: 0px; margin-bottom:0px; margin-left: 0px; margin-right: 0px; background-color: #b59800; color: #FFFFFF;padding-top: 10px; padding-bottom: 10px;", textOutput('textDataNameMods'))),
              navbarPage(span(style = 'color: #000000;','Module'), id = 'navModule',
                         header = hidden(tagList(div(id = "navModuleHeader",
                                                     h4(style = "text-align: center; margin-top: 0px; margin-bottom:5px;  margin-left: 0px; margin-right: 0px; color: #FFFFFF; background-color: #ae6500; padding-top: 10px; padding-bottom: 10px;",
                                                        textOutput('textFiltersMods'))))),
                         #################### Selecting Modules ################
                         tabPanel('Select Modules',
                                  fluidRow(column(4,offset = 4,conditionalPanel(condition = "input.mselectColumnVaccine != null && input.mselectColumnDay != null",
                                                                                actionBttn('mbuttonApplySelection','Apply Filters',style = 'unite', size = 'sm', color = 'warning', block = TRUE),
                                                                                bsTooltip('mbuttonApplySelection',"Apply filters to select modules for plotting. Selected filters are applied in order left → right", placement = "top")
                                  ))),
                                  br(),
                                  wellPanel(style = "background-color: #FFFFFF;",
                                            fluidRow(
                                              column(4,
                                                     conditionalPanel(condition = "input.mselectColumnVaccine != null && input.mselectColumnDay != null",
                                                                      wellPanel(style = "background-color: #feffee;",
                                                                                awesomeCheckbox(status = 'success',inputId = 'mcheckboxSelectKeyword',label =  h4(style = "margin-top: 0px;",'1. Using regex keyword search'), value = FALSE),
                                                                                textInput('mtextInputKeyword',NULL),
                                                                                h4(style = "margin-top: 0px;","Search:"),
                                                                                awesomeRadio(status = 'success', 'mradioKeywordColumn',NULL,choices = c('Title','Module'), inline = TRUE),
                                                                                bsTooltip("mradioKeywordColumn", "Search in Module Names or Module Titles"),
                                                                                conditionalPanel(condition = "input.mradioKeywordColumn == 'Module'",p(style = "color: #44b84b;","Spaces will be stripped")),
                                                                                conditionalPanel(condition = "input.mradioKeywordColumn == 'Title'",p(style = "color: #44b84b;","Spaces will be kept")),
                                                                                awesomeCheckbox("mcheckboxModuleSearchWholeWord","Whole Word", TRUE,status = "danger")
                                                                      ))
                                              ),
                                              column(8,
                                                     wellPanel(style = "background-color: #FFFFFF;",
                                                               h4(style = "text-align: center; margin-top: 0px;", "Select a treatment~time column to filter by value and sort modules by value"),
                                                               fluidRow(
                                                                 column(3,pickerInput('mselectColumnVaccine', choices = NULL, options = list(`style` = "btn-success"))),
                                                                 column(3,pickerInput('mselectColumnDay', choices = NULL, options = list(`style` = "btn-success"))),
                                                                 column(3,awesomeCheckbox(status = 'success', 'mcheckboxDescending', 'Sort Descending', value = TRUE)),
                                                                 column(3,awesomeCheckbox(status = 'success', 'mcheckboxModuleMedians', 'Medians Not Means', value = FALSE))
                                                               ),
                                                               fluidRow(
                                                                 fluidRow(
                                                                   column(6,
                                                                          conditionalPanel(condition = "input.mselectColumnVaccine != null && input.mselectColumnDay != null",
                                                                                           wellPanel(style = "background-color: #feffee;",
                                                                                                     awesomeCheckbox(status = 'success', inputId = 'mcheckboxSelectValues', label =  h4(style = "margin-top: 0px;",'2. Values Within Range:'), value = FALSE),
                                                                                                     fluidRow(
                                                                                                       column(6,numericInput("mnumberExpressionMin", "Lowest:", value = 0)),
                                                                                                       column(6,numericInput("mnumberExpressionMax", "Highest:", value = 0))
                                                                                                     ),
                                                                                                     conditionalPanel(condition = "input.mcheckboxModuleMedians == true",p(style = "color: #44b84b;","Limits Applied To Module Medians, Not Gene Values")),
                                                                                                     conditionalPanel(condition = "input.mcheckboxModuleMedians == false",p(style = "color: #44b84b;","Limits Applied To Module Means, Not Gene Values")),
                                                                                                     actionButton('mbuttonResetValuesRangeCol',NULL, icon = icon("tag"), class = 'btn-outline-primary'),
                                                                                                     actionButton('mbuttonResetValuesRangeData',NULL, icon = icon("folder-open"), class = 'btn-outline-primary')
                                                                                           ))),
                                                                   column(6,
                                                                          conditionalPanel(condition = "input.mselectColumnVaccine != null && input.mselectColumnDay != null",
                                                                                           wellPanel(style = "background-color: #feffee;",
                                                                                                     awesomeCheckbox(status = 'success', inputId = 'mcheckboxSelectRows', label =  h4(style = "margin-top: 0px;",'3. Row Numbers Within Range:'), value = TRUE),
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
                                  )
                         ), #tab
                         #     #################### Top Modules #######################
                         tabPanel('Selected Modules',
                                  wellPanel(style = "background-color: #FFFFFF;",
                                            h4(style = "margin-top: 0px;",'Expression Values Of Selected Modules'),
                                            fluidRow(
                                              column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowLegendGenesModules', 'Legend', value = FALSE)),
                                              column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowZeroGenesModules', '0 |----', value = TRUE)),
                                              column(3,style = "margin-top: 10px;",
                                                     awesomeRadio(status = 'success', 'mradioGroupTitleName',NULL,choices = c('Module','Title'),inline = TRUE),
                                                     bsTooltip("mradioGroupTitleName", "Group by Modules or Module Titles (may be fewer)")),
                                              column(3,awesomeCheckbox(status = 'success', 'mcheckboxGGplotGenesModules', 'ggplot2', value = FALSE)),
                                              column(2, offset = 2, style = "margin-top: -10px;", sliderInput("numbermplotSelectedModulesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numbermplotSelectedModulesSIZEheight", "Plot height"))
                                            ),
                                            wellPanel(style = "background-color: #FFFFFF;",
                                                      uiOutput("mplotSelectedModulesSIZE")
                                            ),
                                            fluidRow(
                                              column(1, downloadButton(class="btn-warning",'buttonPNGmplotSelectedModules', 'HiRes PNG')),
                                              column(11,p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Gene(s) Mapping To Module Genes Are Summarised")))
                                  ),
                                  fluidRow(
                                    column(6,
                                            downloadButton(class="btn-outline-primary",'mbuttonSaveTableModules', 'Table'),
                                            downloadButton(class="btn-warning",'buttonSaveTableTopModulesUpPlot', 'Table As PNG'),
                                            downloadButton(class="btn-warning",'buttonSaveTableTopModulesUOnlypPlot', 'Modules List As PNG')),
                                    column(2,pickerInput('pickerSaveListTopModules',label = NULL, width = '100%', choices = c('Module','Title','Category'), options = list(`style` = "btn-danger"))),
                                    column(2,downloadButton(class="btn-danger",'mbuttonSaveListTopModules', 'Download List'), bsTooltip("mbuttonSaveListTopModules", "Download List To Paste Into Regex Keyword Search"))
                                  ),
                                  hr(),
                                  dataTableOutput('mdatatableTopModulesUp')
                         ),
                         #################### Top Modules->Genes ###################
                         tabPanel('Module->Genes',
                                  wellPanel(style = "background-color: #FFFFFF;",
                                            h4(style = "margin-top: 0px;",'Expression Values Of Genes Within Selected Modules'),
                                            fluidRow(
                                              column(5,pickerInput(inputId = 'mselectModuleForGenes', label = NULL, choices = NULL, inline = TRUE,options = list(`style` = "btn-success"))),
                                              column(2, awesomeCheckbox(status = 'success', 'mcheckboxShowMissingModuleGenes', 'Show Missing Genes', value = TRUE)),
                                              column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowLegendModuleGenes', 'Legend', value = FALSE)),
                                              column(1,awesomeCheckbox(status = 'success', 'mcheckboxShowZeroModuleGenes', '0 |----', value = TRUE)),
                                              column(1,awesomeCheckbox(status = 'success', 'mcheckboxGGplotModuleGenes', 'ggplot2', value = FALSE)),
                                              column(2, style = "margin-top: -10px;", sliderInput("mnumberPlotModuleGenesSIZEheight", NULL, value = 400, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numberPlotModuleGenesSIZEheight", "Plot height"))
                                            ),
                                            wellPanel(style = "background-color: #FFFFFF;",
                                                      uiOutput("mplotModuleGenesSIZE")
                                            ),
                                            fluidRow(
                                              column(1,downloadButton(class="btn-warning",'mbuttonPNGplotModuleGenes', 'HiRes PNG')),
                                              column(11,p(style = "margin-top: 10px;color: #44b84b;text-align: center;", "Values Of Individual Gene(s) Mapping To Module Genes Are Summarised"))
                                            )),
                                  downloadButton(class="btn-outline-primary",'mbuttonSaveTableModulesGenes', 'Table'),
                                  downloadButton(class="btn-danger",'mbuttonTopModulesGenesList', 'Module Genes List'), bsTooltip("mbuttonTopModulesGenesList", "Gene List To Paste Into regex Keyword Search"),
                                  hr(),
                                  dataTableOutput('mdatatableModuleGenes')),
                         #################### Top Modules Series #######################
                         tabPanel('Modules:Series',
                                  wellPanel(style = "background-color: #FFFFFF;",
                                            fluidRow(column(3, h4(style = "margin-top: 0px; text-align: center;","Plot Time Course Of Modules")),
                                                     column(9,p(style = "color: #44b84b; text-align: center;", "You cannot paste into the boxes below. Paste any saved lists into the Select Modules regex filter instead"))),
                                            fluidRow(
                                              column(3,
                                                     wellPanel(style = "background-color: #feffee;",
                                                               conditionalPanel(condition = "input.mselectColumnForModuleSeriesVaccines != null && input.mselectColumnForModuleSeriesDays != null &&
                                                                                ((input.radioModulesModulesSeries == 'Filters' && input.mselectPlotModulesInSeries != null) ||
                                                                                (input.radioModulesModulesSeries == 'Titles' && input.mselectModuleTitles != null) ||
                                                                                (input.radioModulesModulesSeries == 'Modules' && input.mselectModuleAllModules != null))",
                                                                                actionBttn('mbuttonPlotModuleSeries','Plot',style = 'unite', size = 'sm', color = 'warning', block = TRUE)
                                                               ),
                                                               conditionalPanel(condition = "input.mselectColumnForModuleSeriesVaccines == null || input.mselectColumnForModuleSeriesDays == null ||
                                                                                ((input.radioModulesModulesSeries == 'Filters' && input.mselectPlotModulesInSeries == null) ||
                                                                                (input.radioModulesModulesSeries == 'Titles' && input.mselectModuleTitles == null) ||
                                                                                (input.radioModulesModulesSeries == 'Modules' && input.mselectModuleAllModules == null))",
                                                                                p(style = "color: #728f17; text-align: center;","Choose Treatment~Times & Modules Or Titles To Plot")),
                                                               fluidRow(
                                                                 column(6,
                                                                        awesomeRadio(status = 'warning', 'mradioRibbonBoxModuleSeries'," ",choices = c('Lines','Boxplot')),
                                                                        awesomeCheckbox(status = 'success', 'mcheckboxShowFacetModuleSeries', 'Split', value = TRUE),
                                                                        conditionalPanel(condition = "input.mradioRibbonBoxModuleSeries == 'Lines'",
                                                                                         conditionalPanel(condition = "input.mcheckboxShowFacetModuleSeries == true",
                                                                                                          awesomeCheckbox(status = 'success', 'mcheckboxShowGridSeries', 'Gridlines', value = TRUE)),
                                                                                         awesomeCheckbox(status = 'success', 'mcheckboxShowPointsSeries', 'Points', value = FALSE),
                                                                                         awesomeCheckbox(status = 'success', 'mcheckboxShowSEModuleSeries', 'SEM', value = FALSE)
                                                                        )
                                                                 ),
                                                                 column(6,
                                                                        awesomeCheckbox(status = 'success', 'mcheckboxShowLegendModuleSeries', 'Legend', value = FALSE),
                                                                        awesomeCheckbox(status = 'success', 'mcheckboxShowZeroModuleSeries', '0 |----', value = TRUE),
                                                                        awesomeRadio(status = 'success', 'mradioGroupTitleNameModuleSeries','Group By:',choices = c('Title','Module'), selected = 'Module'),
                                                                        bsTooltip("mradioGroupTitleNameModuleSeries", "Group by Modules or Module Titles (may be fewer)"),
                                                                        numericInput("numericNumPanelsmplotModuleSeries",NULL,value = 3, min = 1, step = 1),
                                                                        bsTooltip("numericNumPanelsmplotModuleSeries", "Maximum panels per row when Split"),
                                                                        sliderInput("numbermplotModuleSeriesSIZEheight", NULL, value = 600, min = 300, step = 50, ticks = FALSE, max = 2500), bsTooltip("numbermplotModuleSeriesSIZEheight", "Plot height")
                                                                 ))
                                                     )
                                              ),
                                              column(9,
                                                     wellPanel(style = "background-color: #feffee;",
                                                               fluidRow(
                                                                 column(6,
                                                                        fluidRow(
                                                                          column(8, selectInput('mselectColumnForModuleSeriesVaccines', label = "Treatment", choices = character(0), multiple = TRUE)),
                                                                          column(4, div(style = "margin-top: 20px;",
                                                                                        actionButton('mbuttonAddAllColumnsModuleSeriesVaccines','All', class="btn-outline-primary"),
                                                                                        actionButton('mbuttonRemoveAllColumnsModuleSeriesVaccines','None'))))
                                                                 ),
                                                                 column(6,
                                                                        fluidRow(
                                                                          column(8, selectInput('mselectColumnForModuleSeriesDays', label = "Times", choices = character(0), multiple = TRUE)),
                                                                          column(4, div(style = "margin-top: 20px;",
                                                                                        actionButton('mbuttonAddAllColumnsModuleSeriesDays','All', class="btn-outline-primary"),
                                                                                        actionButton('mbuttonRemoveAllColumnsModuleSeriesDays','None'))))
                                                                 )
                                                               )
                                                     ),
                                                     div(
                                                       h4(style = "margin-top: 0px;","Select Modules To Plot From One Of The Options Below:"),
                                                       radioGroupButtons('radioModulesModulesSeries', NULL,
                                                                         choiceValues = list('Filters', 'Modules','Titles'),
                                                                         choiceNames = list(' Modules You Selected With Filters', ' All Modules In Dataset',' All Module Titles In Dataset'),
                                                                         individual = FALSE, justified = TRUE, status = "primary",
                                                                         checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                                                       style = "color: gold"),
                                                                                          no = tags$i(class = "fa fa-circle-o",
                                                                                                      style = "color: white"))
                                                       ),
                                                       conditionalPanel(condition = "input.radioModulesModulesSeries == 'Filters'",
                                                                        wellPanel(style = "background-color: #feffee;",
                                                                                  fluidRow(
                                                                                    column(8, selectInput('mselectPlotModulesInSeries', label = 'Modules Selected By Filters', character(0), multiple = TRUE)),
                                                                                    column(4, style = "margin-top: 20px;", actionButton('mbuttonAddAllModuleSeries','All', class="btn-outline-primary"),
                                                                                           actionButton('mbuttonRemoveAllModuleSeries','None'))
                                                                                  )
                                                                        )),
                                                       conditionalPanel(condition = "input.radioModulesModulesSeries == 'Modules'",
                                                                        wellPanel(style = "background-color: #dcefa0;",
                                                                                  fluidRow(
                                                                                    column(9,selectInput('mselectModuleAllModules', label = 'Modules In Dataset', character(0), multiple = TRUE)),
                                                                                    column(1,style = "margin-top: 20px;",  actionButton('mbuttonRemoveAllModulesModuleSeries','None')),
                                                                                    column(2,style = "margin-top: 20px;",  conditionalPanel(condition = "input.mselectModuleAllModules != null",
                                                                                                                                            downloadButton(class="btn-danger",'mbuttonSaveListTopModulesSeries', 'Modules List'), bsTooltip("mbuttonSaveListTopModulesSeries", "Modules List To Paste Into regex Keyword Search")
                                                                                    ))
                                                                                  )
                                                                        )),
                                                       conditionalPanel(condition = "input.radioModulesModulesSeries == 'Titles'",
                                                                        wellPanel(style = "background-color: #dcefa0;",
                                                                                  fluidRow(
                                                                                    column(9, selectInput('mselectModuleTitles', label = 'Titles In Dataset', character(0), multiple = TRUE)),
                                                                                    column(1,style = "margin-top: 20px;", actionButton('mbuttonRemoveAllModuleTitles','None')),
                                                                                    column(2,style = "margin-top: 20px;", conditionalPanel(condition = "input.mselectModuleTitles != null",
                                                                                                                                           downloadButton(class="btn-danger",'mbuttonSaveListTopModuleTitlesSeries', 'Titles'), bsTooltip("mbuttonSaveListTopModuleTitlesSeries", "Titles List To Paste Into regex Keyword Search")
                                                                                    ))
                                                                                  )
                                                                        ))
                                                     )
                                              )
                                  ),
                                  wellPanel(style = "background-color: #FFFFFF;",
                                            uiOutput("mplotModuleSeriesSIZE")
                                  ),
                                  fluidRow(conditionalPanel(condition = "output.mplotModuleSeries != null",
                                                            column(1,downloadButton(class="btn-warning",'buttonPNGmplotModuleSeries', 'HiRes PNG')),
                                                            column(8, offset = 1,
                                                                   conditionalPanel(condition = "input.mradioRibbonBoxModuleSeries == 'Lines' && output.mplotModuleSeriesBRUSH == null",p(style = "text-align: center; color:#b1cd46;","Click points to identify, drag to select")),
                                                                   tableOutput("mplotModuleSeriesBRUSH")),
                                                            column(2,textOutput("mplotModuleSeriesGENEMOD"))
                                                            )
                                  ),
                                  conditionalPanel(condition = "output.mdatatableModuleSeries != null",
                                                   downloadButton(class="btn-outline-primary",'mbuttonSaveTableModulesSeries', 'Table')), hr(),
                                  dataTableOutput('mdatatableModuleSeries')
                         )
              )
     )), #explore by module
     ###########   Lookup  ##########
     tabPanel('Lookup',
              navbarPage(span(style = 'color: #000000;','Lookup'), id = 'navLookup',
                         #################### Gene Lookup ###################
                         tabPanel('Probe-Gene Lookup',
                                  wellPanel(style = "background-color: #feffee;",
                                            h4(style = "margin-top: 0px;","Enter a probe or gene name or partial name and click Lookup"),
                                            h5("Use commas to separate multiple names. Alternatively, leave box empty and click Lookup to return all probes & genes, then use search boxes above/below table to search"),
                                            fluidRow(
                                              column(2,style = "margin-top: 0px;",
                                                     pickerInput('pickerGeneProbeLookup',label = NULL, choices = c('Gene','Spot','ProbeName','SystematicName','Description'), options = list(`style` = "btn-success"))),
                                              column(5,textInput('textInputGeneLookup',NULL)),
                                              column(2,awesomeCheckbox("checkboxGeneLookupWholeWord","Whole Word", FALSE,status = "danger")),
                                              column(3,div(actionButton("buttonGeneLookup", "Lookup",class = "btn-success"),actionButton("buttonGeneLookupNone", "Clear")))
                                            ),
                                            h5("The search uses regex, but ignores case and always removes spaces")
                                  ),
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
                                              column(4,textInput('mtextInputModLookup',NULL)),
                                              column(2,awesomeCheckbox("mcheckboxModuleLookupWholeWord","Whole Word", FALSE,status = "danger")),
                                              column(3,div(actionButton("mbuttonModLookup", "Lookup",class = "btn-success"),
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
    ###########   Network  ##########
    tabPanel(value = 'Network', title = "Network",
      wellPanel(
        fluidRow(
         column(2,pickerInput('selectVaccNet', choices = NULL, options = list(`style` = "btn-success"))),
         column(1,pickerInput('selectDayNet', choices = NULL, options = list(`style` = "btn-success"))),
         column(1,actionButton('buttonAddVacDayNet','Add', class = 'btn-warning')),
         column(3, selectInput('selectVacDaysToNet',label = NULL, choices = character(0), multiple = TRUE)),
         column(1,actionButton('buttonClearNet','None')),
         column(1,conditionalPanel(condition = "input.selectVacDaysToNet != null",actionButton('buttonPlotNet','Plot', class = 'btn-warning btn-block'))),
         column(1, numericInput("numericNumRowsNet",NULL,value = 10, min = 1, step = 1),
                bsTooltip("numericNumRowsNet","Number of rows to match", placement = 'top')),
         column(2, awesomeCheckbox('checkboxDescNet', "Descending Value", value = TRUE, status = "success"))
      )),
      wellPanel(
        fluidRow(
          column(2,
                 radioGroupButtons('radioNetType', NULL,
                                   choiceValues = list('spring', 'groups','circle'),
                                   choiceNames = list('Spring', 'Groups','Circle'),
                                   individual = TRUE, justified = TRUE, status = "primary"
                 )),
          column(1,
                 sliderInput("nodeAlphaNet", NULL, value = 0.9, min = 0.1, max = 1, step = 0.1, ticks = FALSE),
                 bsTooltip("nodeAlphaNet", "Node transparency")),
          column(4, offset = 1,
                 radioGroupButtons('radioEdgeCountThreshold', NULL, selected = 'a',
                                   choiceValues = (list('a', 'u','c','v')),
                                   choiceNames = (list('All', 'Unique', 'Common', 'Connections >')),
                                   individual = TRUE, justified = TRUE, status = "danger"
                 )),
          column(1, style = "margin-top: 2px;", conditionalPanel(condition = "input.radioEdgeCountThreshold == 'v'",
                 numericInput("numericEdgeCountThreshold",NULL,value = 2, min = 2, step = 1))),
          column(2,offset = 1,
                 sliderInput("plotNetSIZEheight", NULL, value = 600, min = 300, max = 2500, step = 50, ticks = FALSE),
                 bsTooltip("plotNetSIZEheight", "Click Plot to redraw graph after changing plot height"))
        ),
        fluidRow(
          column(2,
                 radioGroupButtons('radioVennNetworkeNet', NULL,
                                   choiceValues = list('v', 'n'),
                                   choiceNames = list('Venn', 'Network'),
                                   individual = TRUE, justified = TRUE, status = "primary"
                 )),
          column(2,
                 radioGroupButtons('radioLineLabelVariableNet', NULL,
                                   choiceValues = list('MeanValue', 'revrank'),
                                   choiceNames = list('Value', 'Rank'),
                                   individual = TRUE, justified = TRUE, status = "warning"
                 )),
          column(1,style = "margin-top: 2px;", awesomeCheckbox('checkboxLineLabelsNet', "Labels", value = FALSE, status = "warning")),
          column(1, style = "direction: rtl; margin-top: 2px;", awesomeCheckbox('checkboxThresholdEdgesNet', ":between", value = FALSE, status = "warning")),
          conditionalPanel(condition = "input.checkboxThresholdEdgesNet == true",
            column(1, style = "margin-top: 2px;", numericInput("numericEdgeValueThresholdLo",NULL,value = 0),
                   bsTooltip("numericEdgeValueThresholdLo","Only include genes with connection value above this", placement = 'bottom')),
            column(1,style = "margin-top: 2px;",  numericInput("numericEdgeValueThresholdHi",NULL,value = 0),
                   bsTooltip("numericEdgeValueThresholdHi","Only include genes with connection value below this", placement = 'bottom')),
            column(1,style = "margin-top: 2px;", actionButton('buttonResetEdgeLimitNumericsNet','Min~Max', class = 'btn-outline-primary'))
          )
        ),
        conditionalPanel(condition = "input.radioVennNetworkeNet == 'n'",uiOutput("plotNetSIZE")),
        conditionalPanel(condition = "input.radioVennNetworkeNet == 'v'",plotOutput('plotVenn', height = '500px',width = '100%'))
      ),
      hr(),
      fluidRow(
        column(6,dataTableOutput('datatableEdgeListNet')),
        column(6,dataTableOutput('datatableEdgeCountNet'))
      )
      
      
    ),
    ###########   Cells  ##########
     tabPanel(value = 'Cells', title = span(style = "color: #ffb44d;", "Cells"),
              div(id = "divLoadCells",
                  h4(style = "text-align: center; margin-top: 0px;",'Cells Data Are Not Loaded Automatically. Click The Button To Load White Blood Cells Data'),
                  fluidRow(
                    column(8, offset = 2,
                           wellPanel(style = "background-color: #fff4e5;",
                                     fluidRow(
                                       column(8,offset = 2, 
                                              actionBttn('buttonLoadCells','Load Cells Data',style = 'unite', size = 'sm', color = 'success', block = TRUE)
                                       )
                                     )
                           )))
              ),
              hidden(div(id = "divCells",
                         wellPanel(style = "background-color: #FFFFFF;",
                                   h4(style = "margin-top: 0px;",'Time Course Of White Blood Cell Populations'),
                                   fluidRow(
                                     column(4,
                                            wellPanel(style = "background-color: #fff4e5;",
                                                      conditionalPanel(condition = "input.selectColumnForCellsSeriesVaccines != null && input.selectColumnForCellsSeriesDays != null && input.selectCellsForSeries != null",
                                                                       actionBttn('buttonPlotCellsSeries','Plot',style = 'unite', size = 'sm', color = 'warning', block = TRUE)
                                                      ),
                                                      conditionalPanel(condition = "input.selectColumnForCellsSeriesVaccines == null || input.selectColumnForCellsSeriesDays == null || input.selectCellsForSeries == null",
                                                                       p(style = "color: #728f17; text-align: center;","Choose Treatment~Times & Cells To Plot")),
                                                      fluidRow(
                                                        column(6,
                                                               awesomeRadio(status = 'warning', 'radioRibbonBoxCellsSeries'," ",choices = c(Lines = 'Mean',Boxplot = 'Value')),
                                                               awesomeCheckbox(status = 'warning', 'checkboxShowFacetCellsSeries', 'Split Cells', value = TRUE),
                                                               conditionalPanel(condition = "input.radioRibbonBoxCellsSeries == 'Mean'",
                                                                awesomeCheckbox(status = 'warning', 'checkboxShowFacetVaccsSeries', 'Split Treatments', value = TRUE),
                                                                awesomeCheckbox(status = 'warning', 'checkboxShowGridCellsSeries', 'Gridlines', value = TRUE),
                                                                awesomeCheckbox(status = 'warning', 'checkboxShowPointsCellsSeries', 'Points', value = FALSE),
                                                                prettyRadioButtons('radioCellsErrorType', NULL, choiceValues = list('none','ribbon','errorbar'),
                                                                  choiceNames = list('No Error','Ribbon','Bars'), selected = 'ribbon', inline = FALSE, outline = TRUE, status = "warning"),
                                                                bsTooltip("radioCellsErrorType", "Plot SEM as ribbon, error bars or omitted")
                                                               )
                                                        ),
                                                        column(6,
                                                               awesomeRadio(status = 'danger', 'radioMeanFCCellsSeries'," ",choices = c(`Fold Change` = '.FC',`Cell Count` = '')),
                                                               conditionalPanel(condition = "input.radioMeanFCCellsSeries == '.FC'",
                                                                                awesomeCheckbox(status = 'danger', 'checkboxShowZeroCellsSeries', '• |----', value = TRUE)),
                                                               awesomeCheckbox(status = 'success', 'checkboxFreeYCellsSeries', 'Free Y', value = FALSE),
                                                               awesomeCheckbox(status = 'success', 'checkboxShowLegendSumCellsSeries', 'Summary Legend', value = TRUE),
                                                               awesomeCheckbox(status = 'success', 'checkboxShowLegendAllCellsSeries', 'Plot Legends', value = FALSE),
                                                               conditionalPanel(condition = "input.checkboxShowFacetCellsSeries == true || input.checkboxShowFacetVaccsSeries == true",
                                                               awesomeRadio(status = 'success', 'radioColoursVaccineCells',"Colour By",choices = c(`Cell Types` = 'c',`Vaccines` = 'v'))),
                                                               conditionalPanel(condition = "input.checkboxShowFacetCellsSeries == false && input.checkboxShowFacetVaccsSeries == false",
                                                                p(style = "color: #728f17; text-align: left;","Use Points To Show Vaccines")),
                                                              fluidRow(
                                                                 column(6,
                                                                  numericInput("numericNumPanelsCellsSeries",NULL,value = 3, min = 1, step = 1), 
                                                                  bsTooltip("numericNumPanelsCellsSeries", "Maximum number of panels per plot row")),
                                                                 column(6,
                                                                  sliderInput("numericPlotCellSeriesSIZEheight", NULL, value = 600, min = 300, max = 2500, step = 50, ticks = FALSE),
                                                                  bsTooltip("numericPlotCellSeriesSIZEheight", "Plot height"))
                                                               )
                                                        )
                                                      )
                                            )
                                     ),
                                     column(8,
                                            wellPanel(style = "background-color: #fff4e5;",
                                                      fluidRow(
                                                        column(6,
                                                               fluidRow(
                                                                 column(8, selectInput('selectColumnForCellsSeriesVaccines', label = "Treatment", choices = character(0), multiple = TRUE)),
                                                                 column(4,div(style = "margin-top: 20px;",
                                                                              actionButton('buttonAddAllColumnsCellsSeriesVaccines','All', class="btn-outline-primary"),
                                                                              actionButton('buttonRemoveAllColumnsCellsSeriesVaccines','None')))
                                                               )
                                                        ),
                                                        column(6,
                                                               fluidRow(
                                                                 column(8, selectInput('selectColumnForCellsSeriesDays', label = "Times", choices = character(0), multiple = TRUE)),
                                                                 column(4, div(style = "margin-top: 20px;",
                                                                               actionButton('buttonAddAllColumnsCellsSeriesDays','All', class="btn-outline-primary"),
                                                                               actionButton('buttonRemoveAllColumnsCellsSeriesDays','None'))))
                                                        )
                                                      )
                                            ),
                                            wellPanel(style = "background-color: #fff4e5;",
                                                      fluidRow(
                                                        column(9, selectInput('selectCellsForSeries', label = 'Cell Types', character(0), multiple = TRUE)),
                                                        column(3, div(style = "margin-top: 20px;",
                                                                      actionButton('buttonAddAllCellsCellsSeries','All', class="btn-outline-primary"),
                                                                      actionButton('buttonRemoveAllCellsCellsSeries','None'))
                                                        )
                                                      ))
                                     )
                                   ),
                                   wellPanel(style = "background-color: #FFFFFF;",
                                             uiOutput("plotCellsSeriesSIZE")
                                   ),
                                   fluidRow(conditionalPanel(condition = "output.plotCellsSeries != null",
                                                             column(1,downloadButton(class="btn-warning",'buttonPNGplotCellsSeries', 'HiRes PNG'))
                                   ))
                         ),
                         conditionalPanel(condition = "output.datatableCellsSeries != null",
                                          downloadButton(class="btn-outline-primary",'buttonSaveTableCellsSeries', 'Table')),
                         hr(),
                         dataTableOutput('datatableCellsSeries')
              ))# div
     ),
     ###########   Cytokines  ##########
     tabPanel(value = 'Cytokines', title = span(style = "color: #8df900;", 'Cytokines'),
      div(id = "divLoadCytokines",
          h4(style = "text-align: center; margin-top: 0px;",'Cytokines Data Are Not Loaded Automatically. Click The Button To Load.'),
          fluidRow(
            column(8, offset = 2,
                   wellPanel(style = "background-color: #f3fbe0;",
                             fluidRow(
                               column(8,offset = 2, 
                                      actionBttn('buttonLoadCytokines','Load Cytokines Data',style = 'unite', size = 'sm', color = 'success', block = TRUE)
                               )
                             )
                   )))
      ),
      hidden(div(id = "divCytokines",
       # navbarPage(span(style = 'color: #000000;','Cytokines'), id = 'navCytokines',
       # tabPanel('Plot',
                h4(style = "margin-top: 0px;",'Time Course Of Cytokines'),
                wellPanel(style = "background-color: #f3fbe0;",
                          fluidRow(
                            column(1,style = "margin-top: 20px;",
                                   conditionalPanel(condition = "input.cselectCytokines != null && input.cselectTreatments != null && input.cselectDays != null",
                                                    actionBttn('buttonPlotCytokines','Plot',style = 'unite', size = 'sm', color = 'warning', block = TRUE)
                                   ),
                                   conditionalPanel(condition = "input.cselectCytokines == null || input.cselectTreatments == null || input.cselectDays == null", p(style = "color: #728f17; text-align: center;","Choose Variables To Plot"))
                            ),
                            column(4,selectInput("cselectCytokines", "Cytokines", choices = character(0), multiple = TRUE),div(actionButton('cbuttonAddAllCytokines','All', class="btn-outline-primary"),actionButton('cbuttonAddNoneCytokines','None'))
                            ),
                            column(4,selectInput("cselectTreatments", "Vaccines", choices = character(0), multiple = TRUE),div(actionButton('cbuttonAddAllCytokineTreats','All', class="btn-outline-primary"),actionButton('cbuttonAddNoneCytokineTreats','None'))
                            ),
                            column(3,selectInput("cselectDays", "Times", choices = character(0), multiple = TRUE),div(actionButton('cbuttonAddAllCytokineDays','All', class="btn-outline-primary"),actionButton('cbuttonAddNoneCytokineDays','None'))
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
                            column(2,prettyRadioButtons('cradioCytokinesWrap', NULL, choiceValues = list('TC','CT'), choiceNames = list('Vac↓ Cyt→', 'Cyt↓ Vac→'),
                                                        inline = TRUE, outline = TRUE, status = "success"),bsTooltip("cradioCytokinesWrap", "How to order the panels when plotting: Cytokines across and vaccines downwards, or reverse.")),
                            column(1, style = "margin-top: 10px;",
                                   span(id = "spanccheckboxMonochrome",awesomeCheckbox(status = 'success', 'ccheckboxMonochrome', 'Grey', value = FALSE)),bsTooltip("spanccheckboxMonochrome", "Plot in black & white")),
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
       #) tabPanel
      # ) Navpage
      ))
     ),
     ###########   READ ME  ##########
     tabPanel(value = 'ReadMe', title = span(style = "color: #fefc78;", "ReadMe"), icon = icon('info-circle'),
              tabsetPanel(
                tabPanel("Quick Start",
                         includeHTML("QuickStart.html")
                ),
                tabPanel("Overview",
                         includeHTML("help.html")
                ),
                tabPanel("Datasets",
                         includeHTML("helpDatasets.html")
                ),
                tabPanel("Instructions For Use",
                         includeHTML("help2.html")
                ),
                tabPanel("Acknowledgements…",
                         includeHTML("help3.html")
                )
              )
     ),
     #####
     footer = tagList(
       hr(),
       div(style = "margin-left: 10px; margin-right: 10px; ", img(src = 'surrey.png'), img(src = 'ugent.png'), img(src = 'mpiib.png'),img(src = 'icl.png'), img(align = 'right', src = 'BIOVACSAFE_EEI.png')), #img(align = 'right', src = 'eei.png'),img(align = 'right', src = 'biovacsafe.png')
       hr()
     )
  )# navpage top
)
) #hidden Div
)# tagList top