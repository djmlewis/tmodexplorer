
getGenesForSearch <- function(dframe,searchTerms,variableToSearch,wholeWord){
  if(is.null(dframe) || is.null(searchTerms)) return(NULL)
  
  # strip spaces from genes and probes
  if((!grepl("Description",variableToSearch) && !variableToSearch == "Gene Name") && grepl(' ',searchTerms)) {
    searchTerms <- gsub(" ","",searchTerms)
  }
  
  if(grepl(',',searchTerms)) {
   # multiple searchTerms
    searches <- unlist(strsplit(searchTerms,','))
    if(wholeWord == TRUE) {
      if(grepl("Description",variableToSearch) || variableToSearch == "Gene Name") searches <- paste0("\\b",searches,"\\b")
      else searches <- paste0("^",searches,"$")
    }
    selGenes <- map_dfr(searches, ~ filter_at(dframe,.vars = variableToSearch,
                                              any_vars(grepl(.x,., ignore.case = TRUE)))
    )
    if(nrow(selGenes)>0) {
      selGenes <- distinct(selGenes,`Feature Number`, .keep_all = TRUE)
    }
  } else {
  # single search term
    if(wholeWord == TRUE) {
      if(grepl("Description",variableToSearch) || variableToSearch == "Gene Name") searchTerms <- paste0("\\b",searches,"\\b")
      else searchTerms <- paste0("^",searchTerms,"$")
    }
    # no duplicates possible for single searchTerms term
    selGenes <- filter_at(dframe,.vars = variableToSearch,any_vars(grepl(searchTerms,., ignore.case = TRUE)))
  }
  
  return(selGenes)
}

themeBase <- function(rotate = FALSE) {
  a <- ifelse(rotate,60,0)
  hj <- ifelse(rotate,1,0.5)
  
  t <- theme_bw() + 
    theme(
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(size = 0.4,linetype = "dotted",color = "grey"),
      panel.grid.major.y = element_line(size = 0.3,linetype = "solid",color = "grey"),
      plot.title = element_text(size = 18, face = "bold"),
      strip.background = element_blank(),#element_rect(fill = "#f8ffeb"),
      strip.text = element_text(size = 18,face = 'bold'),
      axis.line.x = element_line(size = 1),
      axis.line.y = element_line(size = 1),
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text.y = element_text(size = 16),
      axis.text.x = element_text(size = 16, angle = a, hjust = hj),
      # legend.title = element_blank(),
      legend.text = element_text(size = 14)
    )
  return(t)
}

truncLabels <- function(lab,doTrunc){
  if(doTrunc == FALSE) lab
  else str_trunc(lab,20, ellipsis = "…")
}

currTimeDateFile <- function(file,suffix){
  paste0(file," ",gsub(":","",as.character(Sys.time())),suffix)
}

makeTextListOfFilteredGenes <- function(filteredDF,listTitle) {
  df <- select(filteredDF,contains(" "))
  datalist <- map_chr(colnames(df),function(cn){
    txt <- paste0(cn,"\n",paste(unique(df[[cn]]), collapse = ','),"\n\n")
  })
  datalist <- paste(listTitle,paste(datalist, collapse = ""),"*************************", gsub(",","\n",paste(datalist, collapse = "")),sep = "\n\n")
  return(datalist)
}
