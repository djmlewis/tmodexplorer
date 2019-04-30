geneinputs <-
  set_names(
    genemenus <-
      c(
        #top row
        "checkboxExcludeLINC",
        "rowsLimitNumeric",
        #keyword
        "checkboxSelectKeyword",
        "radioKeywordIncludeExclude",
        "textInputKeyword",
        "selectKeywordColumn",
        "checkboxGeneSearchWholeWord",
        # value / rows top row
        'selectColumnVaccine',
            #'selectColumnDay',
        'checkboxRowsAnyDay',
        'checkboxProbesGenes',
        'checkboxDescending',
        # Value
        "checkboxSelectValues",
        'radioFilterByRowKinetics',
        # treat time
        "numberExpressionMin",
        "numberExpressionMax",
        # ROWS
        'checkboxSelectRows',
        "numberGenesStart",
        "numberGenesEnd"
      ),
    genemenus
  )
geneinputFuns <-
  set_names(
    c(
      #top row
      "updateAwesomeCheckbox",
      "updateNumericInput",
      #keyword
      "updateAwesomeCheckbox",
      "updateRadioGroupButtons",
      "updateTextInput",
      "updatePickerInput",
      "updateAwesomeCheckbox",
      # value / rows top row
      "updatePickerInput",
          #"updatePickerInput",
      "updateAwesomeCheckbox",
      "updateAwesomeCheckbox",
      "updateAwesomeCheckbox",
      # Value
      "updateAwesomeCheckbox",
      "updateRadioGroupButtons",
      # treat time
      "updateNumericInput",
      "updateNumericInput",
      # ROWS
      "updateAwesomeCheckbox",
      "updateNumericInput",
      "updateNumericInput"
    ),
    genemenus
  )
argNameForInputFun <- function(inpt){
  return(
    case_when(
      inpt %in% c("updateAwesomeCheckbox", "updateNumericInput","updateTextInput") ~ "value",
      inpt %in% c("updateRadioGroupButtons", "updatePickerInput") ~ "selected",
      TRUE ~ "unkown"
    )
  )
}


