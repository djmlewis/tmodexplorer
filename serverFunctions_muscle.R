

themeBaseMuscle <- function(rotate = FALSE) {
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

