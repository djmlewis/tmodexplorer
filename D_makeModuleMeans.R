

library(tools)
library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(tmod)
library(ggplot2)
library(utf8)



geneValuesForModules <- function(fname) {

  expressiondata <- read_rds(paste0(fname, "data.rds"))
  exprCols <- names(expressiondata)[grepl('_', names(expressiondata))]
  
  annotation <- read_rds(paste0(fname, "annotation.rds")) %>%
    select(X1, GeneName)
  
  expressiondata <- expressiondata %>%
    full_join(annotation, by = 'X1')
  
  # pull the gene rows per module and save as raw data
  modulesData <-
    map_dfr(modules, function(mod) {
      print(paste0('Processing...', mod))

      genesInMod <- tmod::getModuleMembers(mod)[[mod]]
      expr <- expressiondata %>%
        filter(GeneName %in% genesInMod) %>%
        mutate(Module = mod) %>%
        select(Module, one_of(exprCols)) %>%
        gather(
          key = 'Column',
          value = 'Value',
          -Module,
          convert = TRUE,
          factor_key = FALSE
        ) %>%
        full_join(modulesAnnot, by = 'Module')
    })
  # save the raw gene expressions in modules
  saveRDS(modulesData, file = paste0(fname, 'modules.rds'))
  
  # summarise the means etc
  
  modulesDataSumm <- modulesData %>%
    group_by(Module, Column) %>%
    summarise(
      N = n(),
      Median = median(Value, na.rm = TRUE),
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      SE = SD/sqrt(N)
    ) %>%
    full_join(modulesAnnot, by = 'Module')
  
  saveRDS(modulesDataSumm, file = paste0(fname, 'modulesMeans.rds'))
  
}


####### SCRIPT

load('tmod.rda')
modules <- tmod$MODULES$ID

modulesAnnot <- tmod$MODULES %>%
  select(Module = ID, Title, Category)

# save the gene expressions for each module for each time point

geneValuesForModules("datafiles/Gent Fluad Limma log2 mean expression/")
geneValuesForModules("datafiles/Gent Fluad Limma log2 mean FI/")


