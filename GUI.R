# Define UI  
ui <- 
  navbarPage('tmodExplorer', id = 'navbarTop', position = "fixed-top", theme = "theme.css", windowTitle = 'tmodExplorer',
             inverse = TRUE,
             header = tagList(tags$style(type="text/css", "body {padding-top: 70px;}")),
  ######################  TABS  #########
    # tabsetPanel(
  #################### Data Load #########################
  tabPanel('Password',
           h4('Please enter the password you have been given to access tmodExplorer'),
           wellPanel(
             fluidRow(
               column(3,passwordInput('password', 'Enter Password To Access Database')),
               column(1,actionButton('buttonPassword','Enter',class = "btn-primary"))
             ),
             p("Please contact d.j.lewis@surrey.ac.uk to request a password")
           )
  ),
  tabPanel('Load data',
     h4("Select pre-loaded dataset to analyse and click 'Load'"),
     fluidRow(
       column(6,
              wellPanel(
                selectInput('selectData', NULL, character(0)),
                actionButton(
                  'buttonLoadData',
                  label = 'Load',
                  class = "btn-primary"
                )
              ))
       # column(6,
       #        wellPanel(
       #          textInput(
       #            'textInputUploadFileName',
       #            label = NULL,
       #            placeholder = 'Dataset Name'
       #          ),
       #          fileInput(
       #            'fileInputUploadData',
       #            label = NULL,
       #            placeholder = 'Choose two files named "data.rds" & "annotation.rds"',
       #            buttonLabel = 'Upload…',
       #            multiple = TRUE,
       #            accept = c(".rds")
       #          )
       #        ))
       ),
     h4(align = 'center', textOutput('textDataName')), hr(),
     dataTableOutput('datatableAll')
  ),
  # ############## PROBES #################
      tabPanel('Explore By Probe',
        h4(align = 'center', textOutput('textDataNameProbes')),
         navbarPage('Probe', id = 'navProbe', header = h4(align = 'center', textOutput('textFiltersProbes')),
                     #################### Selecting  ################
                     tabPanel('Select Probe',
                              wellPanel(
                                h5('Choose a treatment & timepoint column to sort, then some filters. When ready click the button above'),
                                fluidRow(
                                  column(2,selectInput('selectColumn', 'Column To Sort', character(0), width = 400, selectize = F)),
                                  column(1,checkboxInput('checkboxDescending', 'Sort Descending', value = TRUE)),
                                  column(1,checkboxInput('checkboxProbesGenes', 'Gene Averages', value = FALSE))
                                ),
                                div(
                                  h4('Filter probes by a combination of options below'),
                                  h6('Selected filters are applied in order left to right'),
                                  fluidRow(
                                    column(4,wellPanel(
                                      h4(checkboxInput('checkboxSelectKeyword', '1. Using regex', value = FALSE)),
                                      textInput('textInputKeyword',NULL),
                                      h4("Search:"),
                                      radioButtons('radioKeywordColumn',NULL,choices = c('Description','Gene','Probe'))
                                    )),
                                    column(4,wellPanel(
                                      h4(checkboxInput('checkboxSelectValues', '2. Sorted Column Values Within Range:', value = FALSE)),
                                      numericInput("numberExpressionMin", "Lowest:", value = 0), 
                                      numericInput("numberExpressionMax", "Highest:", value = 0),
                                      actionButton('buttonResetValuesRangeCol','Column', class = 'btn-outline-primary'),
                                      actionButton('buttonResetValuesRangeData','Data', class = 'btn-outline-primary')
                                    )),
                                    column(4,wellPanel(
                                      h4(checkboxInput('checkboxSelectRows', '3. Sorted Column Row Numbers', value = TRUE)),
                                      numericInput("numberGenesStart", "From Row:", 0, min = 0, max = 200, step = 5), 
                                      numericInput("numberGenesEnd", "To Row:", 10, min = 0, max = 200, step = 5),
                                      p("More than 100 rows will result in slow response")
                                    ))
                                  )
                                ),
                                hr(),
                                actionButton('buttonApplySelection','Apply Selections',class = "btn-primary")
                              )
                     ),
                     #################### Top Probes #######################
                     tabPanel('Selected Probes',
                       wellPanel(
                         h4('Probes Or Genes Meeting The Filters, Sorted By Values In Selected Treatment-Time Column'),
                         div(downloadButton('buttonSaveTableProbes', 'Download Table'),
                        downloadButton('buttonSaveListGenes', 'Download Gene List'),
                        downloadButton('buttonSaveListProbes', 'Download Probe List')),
                       hr(),
                         dataTableOutput('datatableTopGenesUp'))
                     ),
                     #################### Top Probes Series ################
                     tabPanel('Probes:Series',
                       wellPanel(
                         h4('Plot Time Course Of Probes Or Genes Meeting The Filters, By Treatment-Time Column'),
                         h5("Select Some Treatment-Timepoint Columns And Click Plot"),
                         fluidRow(
                           column(2,
                                  wellPanel(
                                    actionButton('buttonPlotSeries','Plot',class = "btn-primary"),
                                    checkboxInput('checkboxSplitSeries', 'Split', value = TRUE),
                                    radioButtons('radioBoxLineProbesSeries',NULL, choices = c('Points','Boxplot')),
                                    conditionalPanel(condition = "input.radioBoxLineProbesSeries == 'Points'",
                                      checkboxInput('checkboxConnectSeries', 'Connect Points', value = TRUE)
                                    ),
                                    checkboxInput('checkboxShowLegendSeries', 'Legend', value = TRUE),
                                    checkboxInput('checkboxShowZeroSeries', 'Zero', value = TRUE)
                                  )),
                           column(10,
                                  wellPanel(
                                    selectInput('selectColumnsForSeries', label = "Click In Box To Select Columns", choices = character(0), multiple = TRUE),
                                    div(actionButton('buttonAddAllProbesSeries','All', class="btn-outline-primary"),
                                        actionButton('buttonRemoveAllProbesSeries','None'))
                                  )
                           )
                         ),
                         plotOutput('plotTopGenesSeries', height = '600px')),
                       wellPanel(
                         downloadButton('buttonSaveTableProbesSeries', 'Download Table'), hr(), 
                         dataTableOutput('datatableTopGenesSeries'))
                     ),
                     #################### Genes->Modules ##################
                     tabPanel('Genes->Modules',
                       wellPanel(
                         h4('Modules Associated With Selected Probes or Genes'),
                         downloadButton('buttonSaveTableGenesModules', 'Download Table'), hr(), dataTableOutput('datatableGenesModules'))
                     ),
                     #################### Modules #########################
                     tabPanel('Modules',
                       wellPanel(
                         h4('Expression Values Of Modules Associated With Selected Probes / Genes'),
                         fluidRow(
                           column(1,checkboxInput('checkboxShowLegendGenesModules', 'Legend', value = TRUE)),
                           column(1,checkboxInput('checkboxShowZeroGenesModules', 'Zero', value = TRUE)),
                           column(1,checkboxInput('checkboxGGplotGenesModules', 'ggplot2', value = FALSE)),
                           column(3,checkboxInput('checkboxShowPsuedoModuleGenesModules', 'Include Selected As Module', value = TRUE))
                         ),
                         plotOutput('plotGenesModules', height = '800px')),
                       wellPanel(
                         downloadButton('buttonSaveTableModules', 'Download Table'), hr(), dataTableOutput('datatableSelModulesOnly'))
                     ),
                     #################### Modules->Genes ###################
                     tabPanel('Module->Genes',
                        wellPanel(
                          h4('Select A Module From The Menu To View Values Of Its Genes'),
                          selectInput('selectModuleForGenes', NULL, character(0), width = '500px'),
                         fluidRow(
                           column(1,checkboxInput('checkboxShowLegendModuleGenes', 'Legend', value = TRUE)),
                           column(1,checkboxInput('checkboxShowZeroModuleGenes', 'Zero', value = TRUE)),
                           column(1,checkboxInput('checkboxGGplotModuleGenes', 'ggplot2', value = FALSE))
                         ),
                         plotOutput('plotModuleGenes', height = '600px')),
                       wellPanel(
                         downloadButton('buttonSaveTableModulesGenes', 'Download Table'), hr(), dataTableOutput('datatableModuleGenes'))
                     ),
                     #################### Modules Series ###################
                     tabPanel('Modules:Series',
                      wellPanel(
                        h4('Plot Time Course Of Modules Associated With Selected Probes / Genes'),
                        h5('Select Some Treatment-Timepoint Columns And Modules, And Click Plot'),
                        fluidRow(
                           column(2,
                                  wellPanel(
                                    actionButton('buttonPlotModuleSeries','Plot',class = "btn-primary"),
                                    radioButtons('radioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Ribbon')),
                                    conditionalPanel(condition = "input.radioRibbonBoxModuleSeries == 'Ribbon'",
                                      checkboxInput('checkboxShowSEModuleSeries', 'Ribbon+SE', value = FALSE)
                                    ),
                                    checkboxInput('checkboxShowFacetModuleSeries', 'Split', value = TRUE),
                                    checkboxInput('checkboxShowLegendModuleSeries', 'Legend', value = TRUE),
                                    checkboxInput('checkboxShowZeroModuleSeries', 'Zero', value = TRUE)
                                    
                                  )
                           ),
                           column(5,
                                  wellPanel(
                                    selectInput('selectColumnForModuleSeries', label = 'Click In Box To Select Columns', character(0), multiple = TRUE),
                                    div(actionButton('buttonAddAllColumnsModuleSeries','All', class="btn-outline-primary"),
                                    actionButton('buttonRemoveAllColumnsModuleSeries','None'))
                                  )),
                           column(5,
                                  wellPanel(
                                    selectInput('selectModuleForSeries', label = 'Click In Box To Select Modules', character(0), multiple = TRUE),
                                    fluidRow(
                                    column(4,actionButton('buttonAddAllModulesModuleSeries','All', class="btn-outline-primary"),
                                      actionButton('buttonRemoveAllModulesModuleSeries','None')),
                                    column(8,checkboxInput('checkboxShowPseudoModuleModuleSeries', 'Include Selected As Module', value = TRUE))
                                    )
                                  )
                           )
                         ),
                       wellPanel(
                         plotOutput('plotModuleSeries', height = '600px')
                       ),
                       wellPanel(
                         downloadButton('buttonSaveTableModulesSeries', 'Download Table'), hr(), dataTableOutput('datatableModuleSeries'))
                     )),
                    #################### Gene Lookup ###################
                    tabPanel('Gene Lookup',
                      wellPanel(
                        h5("Enter a gene name or partial name and click Lookup"),
                        fluidRow(
                          column(8,textInput('textInputGeneLookup',NULL)),
                          column(4,div(actionButton("buttonGeneLookup", "Lookup",class = "btn-primary"),actionButton("buttonGeneLookupNone", "Clear")))
                        ),
                        h6("Alternatively, leave box empty and click Lookup to return all genes, then use search boxes above/below table to search"),
                        dataTableOutput('datatableGeneLookup')
                      )
                    )
                    
         ) # tabset Panel in explore by probe
    ),
  ############## MODULES #################
  tabPanel('Explore By Module',
    h4(align = 'center', textOutput('textDataNameMods')),
    navbarPage('Module', id = 'navModule', header = h4(align = 'center', textOutput('textFiltersMods')),
      #################### Selecting Modules ################
      tabPanel('Select Modules',
      wellPanel(
        inputPanel(
          selectInput('mselectColumn', 'Column To Sort', character(0), width = 400, selectize = F),
          checkboxInput('mcheckboxDescending', 'Sort Descending', value = TRUE),
          checkboxInput('mcheckboxModuleMedians', 'Use Medians Not Means', value = FALSE)
        ),
        h4('Filter modules by a combination of options below'),
        h6('Selected filters are applied in order left to right'),
        inputPanel(
          wellPanel(
            checkboxInput('mcheckboxSelectKeyword', h5('1. Using regex'), value = FALSE),
            textInput('mtextInputKeyword',NULL),
            h5("Search:"),
            radioButtons('mradioKeywordColumn',NULL,choices = c('Title','Module'))
          ),
          wellPanel(
            checkboxInput('mcheckboxSelectValues', h5('2. Sorted Column Values Within Range:'), value = FALSE),
            numericInput("mnumberExpressionMin", "Lowest:", value = 0),
            numericInput("mnumberExpressionMax", "Highest:", value = 0),
            actionButton('mbuttonResetValuesRangeCol','Column', class = 'btn-outline-primary'),
            actionButton('mbuttonResetValuesRangeData','Data', class = 'btn-outline-primary')
          ),
          wellPanel(
            checkboxInput('mcheckboxSelectRows', h5('3. Sorted Column Row Numbers'), value = TRUE),
            numericInput("mnumberModsStart", "From Row:", 0, min = 0, max = 200, step = 5),
            numericInput("mnumberModsEnd", "To Row:", 10, min = 0, max = 200, step = 5)
          )
        ),
        hr(),
        actionButton('mbuttonApplySelection','Apply Selections',class = "btn-primary")
      )
     ),
    #################### Top Modules #######################
     tabPanel('Selected Modules',
       wellPanel(
         h4('Expression Values Of Selected Modules'),
         fluidRow(
           column(1,checkboxInput('mcheckboxShowLegendGenesModules', 'Legend', value = TRUE)),
           column(1,checkboxInput('mcheckboxShowZeroGenesModules', 'Zero', value = TRUE)),
           column(1,checkboxInput('mcheckboxGGplotGenesModules', 'ggplot2', value = FALSE)),
           column(4,radioButtons('mradioGroupTitleName','Group By',choices = c('Module','Title'),inline = TRUE))
         ),
         plotOutput('mplotSelectedModules', height = '800px')),
       wellPanel(
         div(downloadButton('mbuttonSaveTableModules', 'Download Table'),
             downloadButton('mbuttonSaveListTopModules', 'Download Modules List'),
             downloadButton('mbuttonSaveListTopModuleTitles', 'Download Titles List'),
             downloadButton('mbuttonSaveListTopModuleCategory', 'Download Categories List')
         ), hr(), 
         dataTableOutput('mdatatableTopModulesUp'))
     ),
    #################### Top Modules Series #######################
     tabPanel('Modules:Series',
       wellPanel(
         h4("Plot Time Course Of Modules"),
         fluidRow(
           column(2,
                  wellPanel(
                    actionButton('mbuttonPlotModuleSeries','Plot',class = "btn-primary"),
                    radioButtons('mradioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Ribbon')),
                    conditionalPanel(condition = "input.mradioRibbonBoxModuleSeries == 'Ribbon'",
                      checkboxInput('mcheckboxShowSEModuleSeries', 'Ribbon+SE', value = FALSE)
                      ),
                    checkboxInput('mcheckboxShowFacetModuleSeries', 'Split', value = TRUE),
                    checkboxInput('mcheckboxShowLegendModuleSeries', 'Legend', value = TRUE),
                    checkboxInput('mcheckboxShowZeroModuleSeries', 'Zero', value = TRUE),
                    radioButtons('mradioGroupTitleNameModuleSeries','Group Boxplot',choices = c('Title','Module'), selected = 'Module')
                  )
           ),
           column(10,
                  wellPanel(
                    selectInput('mselectColumnForModuleSeries', label = 'Click In Box To Select Columns To Plot', character(0), multiple = TRUE),
                    div(actionButton('mbuttonAddAllColumnsModuleSeries','All', class="btn-outline-primary"),
                    actionButton('mbuttonRemoveAllColumnsModuleSeries','None'))
                    ),
                  div(
                    h5("Select Modules To Plot Using By Clicking In One Of The Boxes Below:"),
                    h6("You cannot paste into these boxes. Paste any saved lists using the Select tab regex filter"),
                    radioButtons('radioModulesModulesSeries', NULL, inline = TRUE, choiceNames = list(
                      wellPanel(
                        selectInput('mselectPlotModulesInSeries', label = ('Modules Selected By Filters'), character(0), multiple = TRUE),
                        actionButton('mbuttonAddAllModuleSeries','All', class="btn-outline-primary"),
                        actionButton('mbuttonRemoveAllModuleSeries','None')
                      ),
                      wellPanel(
                        selectInput('mselectModuleTitles', label = ('Titles In Datset'), character(0), multiple = TRUE),
                        actionButton('mbuttonRemoveAllModuleTitles','None'),
                        downloadButton('mbuttonSaveListTopModuleTitlesSeries', 'Download Titles List')
                      ),
                      wellPanel(
                        selectInput('mselectModuleAllModules', label = ('Modules In Datset'), character(0), multiple = TRUE),
                        actionButton('mbuttonRemoveAllModulesModuleSeries','None'),
                        downloadButton('mbuttonSaveListTopModulesSeries', 'Download Modules List')
                      )
                    ),
                    choiceValues = list('Modules Selected By Filters','All Titles In The Datset', 'All Modules In The Datset')
                    )
                  )
                )
              ),
         plotOutput('mplotModuleSeries', height = '600px')
       ),
       wellPanel(
         downloadButton('mbuttonSaveTableModulesSeries', 'Download Table'), hr(), 
         dataTableOutput('mdatatableModuleSeries'))
     ),
    #################### Module Lookup #######################
    tabPanel('Module Lookup',
      wellPanel(
        h5("Use the search boxes above and below the table to lookup module, title or category"),
        dataTableOutput('mdatatableModuleLookup')
      )
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
  div(align = 'right',img(src = 'biovacsafe.png'),img(src = 'eei.png')),
  hr()
  )# navpage top
