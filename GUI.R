# Define UI
ui <- 
  fluidPage(
######################  TABS  #########
  tabsetPanel(
#################### Data Load #########################
tabPanel('Load data',
         h4("Select pre-loaded dataset to analyse"),
         fluidRow(
           column(6,
                  wellPanel(
                    selectInput('selectData', NULL, character(0)),
                    actionButton(
                      'buttonLoadData',
                      label = 'Load',
                      style = "background-color: #d4fb78;"
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
         h4(textOutput('textFileName')),
         dataTableOutput('datatableAll')
),
# ############## PROBES #################
    tabPanel('Explore By Probe',
      h4(textOutput('textFileName2')),
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
                       wellPanel(
                         div(downloadButton('buttonSaveTableProbesSeries', 'Table'),
                         downloadButton('buttonSavePlotProbesSeries', 'Plot'))
                       ),
                       wellPanel(
                         fluidRow(
                           column(2,
                                  wellPanel(
                                    actionButton('buttonPlotSeries','Plot',style = "background-color: #d4fb78;"),
                                    checkboxInput('checkboxSplitSeries', 'Split', value = TRUE),
                                    checkboxInput('checkboxConnectSeries', 'Connect', value = TRUE),
                                    checkboxInput('checkboxShowLegendSeries', 'Legend', value = TRUE),
                                    checkboxInput('checkboxShowZeroSeries', 'Zero', value = TRUE)
                                  )),
                           column(10,
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
                           checkboxInput('checkboxShowZeroGenesModules', 'Zero', value = TRUE),
                           checkboxInput('checkboxGGplotGenesModules', 'Use ggplot2', value = FALSE)
                         ),
                         plotOutput('plotGenesModules', height = '800px')),
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
                           checkboxInput('checkboxShowZeroModuleGenes', 'Zero', value = TRUE),
                           checkboxInput('checkboxGGplotModuleGenes', 'Use ggplot2', value = FALSE)
                         ),
                         plotOutput('plotModuleGenes', height = '600px')),
                       wellPanel(dataTableOutput('datatableModuleGenes'))
                     ),
                     #################### Modules Series ###################
                     tabPanel(
                       'Modules:Series',
                       wellPanel(
                         div(downloadButton('buttonSavePlotModulesSeries', 'Plot'),
                         downloadButton('buttonSaveTableModulesSeries', 'Table'))
                       ),
                       fluidRow(
                         column(2,
                                wellPanel(
                                  actionButton('buttonPlotModuleSeries','Plot',style = "background-color: #d4fb78;"),
                                  radioButtons('radioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Ribbon')),
                                  checkboxInput('checkboxShowFacetModuleSeries', 'Split', value = TRUE),
                                  checkboxInput('checkboxShowLegendModuleSeries', 'Legend', value = TRUE),
                                  checkboxInput('checkboxShowZeroModuleSeries', 'Zero', value = TRUE),
                                  checkboxInput('checkboxShowSEModuleSeries', 'Ribbon+SE', value = FALSE)
                                )
                         ),
                         column(5,
                                wellPanel(
                                  selectInput('selectColumnForModuleSeries', label = 'Columns', character(0), multiple = TRUE),
                                  div(actionButton('buttonAddAllColumnsModuleSeries','All'),
                                  actionButton('buttonRemoveAllColumnsModuleSeries','None'))
                                )),
                         column(5,
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
         h4(textOutput('textFileNameMods')),
         tabsetPanel(
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
          radioButtons('mradioKeywordColumn',NULL,choices = c('Title','Module'))
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
       div(downloadButton('mbuttonSavePlotModules', 'Plot'),
       downloadButton('mbuttonSaveTableModules', 'Table'))
     ),
     h4('Expression Values Of Selected Modules'),
     wellPanel(
       inputPanel(
         checkboxInput('mcheckboxShowLegendGenesModules', 'Legend', value = TRUE),
         checkboxInput('mcheckboxShowZeroGenesModules', 'Zero', value = TRUE),
         radioButtons('mradioGroupTitleName','Group By',choices = c('Title','Module'),inline = TRUE)
       ),
       plotOutput('mplotSelectedModules', height = '800px')),
     wellPanel(dataTableOutput('mdatatableTopModulesUp'))
   ),
  #################### Top Modules Series #######################
   tabPanel(
     'Modules:Series',
     wellPanel(
       div(downloadButton('mbuttonSavePlotModulesSeries', 'Plot'),
       downloadButton('mbuttonSaveTableModulesSeries', 'Table'))
     ),
     fluidRow(
       column(2,
              wellPanel(
                actionButton('mbuttonPlotModuleSeries','Plot',style = "background-color: #d4fb78;"),
                radioButtons('mradioRibbonBoxModuleSeries',NULL,choices = c('Boxplot','Ribbon')),
                checkboxInput('mcheckboxShowFacetModuleSeries', 'Split', value = TRUE),
                checkboxInput('mcheckboxShowLegendModuleSeries', 'Legend', value = TRUE),
                checkboxInput('mcheckboxShowZeroModuleSeries', 'Zero', value = TRUE),
                checkboxInput('mcheckboxShowSEModuleSeries', 'Ribbon+SE', value = FALSE),
                radioButtons('mradioGroupTitleNameModuleSeries','Group Boxplot',choices = c('Title','Module'), selected = 'Module')
              )
       ),
       column(10,
              wellPanel(
              fluidRow(
                column(6,
                selectInput('mselectColumnForModuleSeries', label = 'Columns To Plot', character(0), multiple = TRUE),
                div(actionButton('mbuttonAddAllColumnsModuleSeries','All'),
                actionButton('mbuttonRemoveAllColumnsModuleSeries','None'))),
                
                column(6,
                div(style = "color: #008f51;",
                selectInput('mselectPlottedModuleForSeries', label = 'Modules To Plot', character(0), multiple = TRUE),
                div(actionButton('mbuttonRemoveAllPlottedModulesModuleSeries','None'))))
              )
              ),
              h4("Select Modules To Plot Using The Menus Below:", style = "color: #008f51;"),
              wellPanel(
                div(style = "color: #008f51;",
                fluidRow(
                  column(4,
                    selectInput('mselectModuleForSeries', label = 'Modules Selected By Filters', character(0), multiple = TRUE),
                    actionButton('mbuttonSetSelectedModulesAsModuleSeries','Set As Plotted Modules'),
                    actionButton('mbuttonAddAllModulesModuleSeries','All'),
                    actionButton('mbuttonRemoveAllModulesModuleSeries','None')
                  ),
                  column(4,
                    selectInput('mselectModuleTitles', label = 'All Titles In The Datset', character(0), multiple = TRUE),
                    actionButton('mbuttonAddTitles','Set As Plotted Modules'),
                    actionButton('mbuttonRemoveAllModuleTitles','None')
                  ),
                  column(4,
                    selectInput('mselectModuleAllModules', label = 'All Modules In The Datset', character(0), multiple = TRUE),
                    actionButton('mbuttonAddAllModules','Set As Plotted Modules'),
                    actionButton('mbuttonRemoveAllModules','None')
                  )
                ))
              )
       )
     ),
     wellPanel(plotOutput('mplotModuleSeries', height = '600px')),
     wellPanel(dataTableOutput('mdatatableModuleSeries'))
   )
 )
    ),
###########   READ ME  ##########
tabPanel(
  'ReadMe',
  h3("Introduction"),
  p("This is beta software and is prone to bugs and crashes. Plots and data tables may take some time to appear - be patient! 
    The more rows you select for plotting the slower it will be. ggplot is not good at providing progress updates."),
  p("The purpose of this app is just to visualise the expression of genes after immunisation, either alone or when clustered into modules.
  It depends on the R package tmod 'Feature Set Enrichment Analysis for Metabolomics and Transcriptomics', created by January Weiner 
    (http://bioinfo.mpiib-berlin.mpg.de/tmod/). I have reverse engineered some aspects of tmod and any errors arising from this are mine. 
    Consult the tmod documentation for a description of the modules."),
  p("The transcriptomics datasets are from the BIOVACSAFE project (www.biovacsafe.eu), and were generated by the group of Stefan Kaufmann at MPIIB, 
Berlin and provided by Jeroen Maertzdorf and January Weiner. 
    I have generated the probe averages per time point and any errors there are mine. The trials were conducted at the University of Gent by Geert Leroux-Roels"),
  p("Source files for the shiny app are available at www.github.com/djmlewis/tmodexplorer. The shiny app is online at djmlewis.shinyapps.io/tmodexplorer"),
  p("Plots can be copied or saved by right-clicking. Plots and data tables can be downloaded by clicking the download button. 
    The plotting parameters are not sophisticated but the data used to create the plot are in the table below and can be saved and imported into R."),
  h2("Instructions For Use"),
  h3('Load Data'),
  h5("You must load a dataset before doing anything. Some are already uploaded to the server."),
  p("Select a dataset. Click Load. All existing plots and datatables will be cleared."),
  h3("Explore By Probe"), 
  p("The idea is to choose a time point and a treatment, and then sort the probes based on their values (gene expression or fold-increase) in that column, after filtering by keyword search or value."),
  h4("Select"),
  p("You must click the green 'Click To Apply Selections Below' button whenever you amend a selection to apply the selection."),
  p("Pick a column to sort that has the treatment-time combination you want to explore. Sort descending for highest values in top rows. Select Gene Averages to have expressions of individual probes that map to a gene averaged into one row, otherwise it will be one probe per row and genes may repeat."),
  h5("Filters"),
  p("Filters are applied left to right - first the keyword search, then the value restriction, finally which rows to include.
    Keyword search uses regex, with some limitations. Multiple terms can be entered separated by commas and are OR'd. 
Do not include spaces (especially after the comma) or quotation marks unless you want them included in the search.
    You can enter partial terms eg interferon finds interferon and interferons. Case is ignored. Commas are interpreted as search term dividers and cannot be 'escaped'.
Select whether to search gene Description or Gene name."),
  p("Max/min expression values are pre-entered into the range boxes when selecting a column, using floor/ceil and so may exceed the actual values. 
Click Column to reset to max/min for selected column, or Data for max/min of whole dataset. 
Value restrictions apply only to values in the column selected for sort. Restrict rows to have top or bottom rows, or a slice. 
    Not restricting number of rows will lead to a very slow refresh while plots and datatables are prepared. If there are not enough rows after the previous selections you will get a warning."),
  h4('Probes'),
  p("A data table showing the probes / genes (if averaged) that meet the filter settings. Use the search boxes within the table to further restrict the rows for viewing in the table, but this has no effect on other tabs."),
  h4("Probes:Series"),
  p("This allows the probes selected on the basis of a single treatment-time column to be plotted for multiple treatment-time columns. 
Click 'All' to enter all columns, or select one by one from the menu. Click 'None' to clear.
    The order entered into the box is respected for all boxes like this. Use the cursor to move between selections to add or delete a column at that point."),
  p("Split: the data are split by treatment using facet_wrap. This works if column names are in the format VVV_TTT where VVV is a treatment name and TTT is an integer value for time point. 
    The app splits the column name by '_' and facet_wraps by VVV while plotting TTT on the x axis, as a factor for box plots or a continuous variable for ribbon plots."),
  h4("Genes->Modules"),
  p("This panel lists the selected probes/genes and shows which - if any - modules contain this gene. Some selected genes may not be included in any modules."),
  h4("Modules"),
  p("A summary of the values of the individual genes contained in the modules associated with the selected probes/genes is plotted automatically when the treatment-time column selection changes. 
The number of probes per gene is also shown in the table (N)."),
  h4("Modules->Genes"),
  p("This allows an inspection of the modules, one by one, of all the genes they contain. 
    The menu lists the modules associated with selected genes/probes. As each gene may have several probes a summary boxplot of the expression values of the probes mapping to the genes inside the module selected in the menu is plotted automatically when the selected module changes 
    Note that a module may contain genes that are not in the selection. Genes within a module that are also in your selection are denoted by the ︎► symbol next to the Y axis, and in the datatable. You may be surprised by how few genes in a module overlap with your selected genes.
    All boxplots use ggplot geom_boxplot with default values."),
  h4("Modules:Series"),
  p("Whereas 'Modules' and 'Modules->Genes' only show the expression values of probes in the one selected treatment-time column, 
    this panel allows the expression of the associated modules to be plotted for all treatment-time columns. 
    To avoid delays when selecting options, nothing is plotted or re-plotted until the green 'Plot' button is clicked. 
Add treatment-time points to the Columns menu. Add Modules to the Modules menu. Split - uses facet_wrap to separate treatments. 
    Ribbon - plots an x-y line curve of the mean expression for a module, add SE range for the probes/genes using 'Ribbon+SE'."),
  h3("Explore By Module"),
  p("The concept is the same as Explore By Probe - read the sections above. However, here the rows are sorted by the mean or median value of the modules, and whole modules are selected using the filters.
    The modules contain only a fraction of all the genes in the probeset, but this tab allows for an analysis of responses of the modules included in tmod."),
  h4("Select"),
  p("Like Explore By Probe above, you select a column containing the trreatment-time combination you want to explore. 
    Filter using the filters (left to right) and sort ascending or descending - default uses the module means, select to sort by medians if you prefer but the ribbon plots still show means."),
  h4("Selected Modules"),
  p("The modules that meet the filter parameters for the selected column are automatically plotted as boxplots for the probes within that module."),
  h4("Modules:Series"),
  p("Select treatment-time columns and plot modules as a time series, optionally split by treatment. Select which modules to plot using the menus underneath: 
    1. The modules selected by the column filter, 2. All module Titles (which group some modules into functional categories) in the dataset, or 3. All modules in the dataset. 
    Once you have selected some modules using one of those menus, click the 'Set As Plotted Modules' button to copy those modules to Selected For Plotting. Click 'Plot' after any changes to modules or options to re-draw the plot."),
  br(),
  h2("Versions"),
  p("1.01. Added menus to Explore By Module / Modules:Series to select modules by Title or individually for plotting, in addition to ones selected by filters based on sorted column."),
  br(),
  p("© David JM Lewis www.djml.eu 2018. E&OE."),br()
)
##### 
  ),# top tabset
h3("Version 1.01 beta 15FEB2018")
)# fluidpage
