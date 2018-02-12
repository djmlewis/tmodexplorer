# file paths need to be localised


calcFImean <- function() {
  rna <- read_rds(paste0("D2/", fname,'.rds')) %>%
    # filter out control probes
    filter(X1 %in% notControlProbes)
  
  # pass the vacdaysRep and vaccinesRep to a walk
  rnamean <- map2_dfc(vacdaysRep,vaccinesRep, function(day,vacc) {
    # extract the rows in the dm dataframe with our vacdays
    dm4vacDay <- filter(dm, VACCDY == day, ACTARMCD == vacc)
    # extract the column names for this day
    cols4vacDay <- dm4vacDay$ColName
    # extract the rna cols using the names
    rna4vacDay <- rna %>%
      select(one_of(cols4vacDay))
    print(paste0('Calculating ',fname,'...',vacc,' - ',day,' cols = ',length(cols4vacDay),' genes = ',nrow(rna4vacDay)))
    # print(cols4vacDay)
    df <- data.frame(rowMeans(rna4vacDay))
    names(df) <- c(paste0(vacc,'_',day))
    return(df)
  })
  # add the probe IDs
  rnamean <- bind_cols(rna[1],rnamean)
  
  print('Writing...')
  write_rds(rnamean, paste0("D2/", fname, "_DAYMEAN.rds"))
  return(rnamean)
}

################## SCRIPT
rm(list = setdiff(ls(), lsf.str()))

# load DM 
dm <- read_rds("D2/outputs/targets_DM.rds") %>%
  mutate(ACTARMCD = as.factor(ACTARMCD))

#   filter out control wells
# annotation.rds must be present in the outputs folder. copy there after saving it manually from the originals
notControlProbes <- read_rds("D2/outputs/annotation.rds") %>%
  filter(ControlType == 0)
notControlProbes <- notControlProbes[['X1']]

# extract the unique subject IDs and ines
vaccdays <- sort(unique(dm$VACCDY))
vaccines <- unique(dm$ACTARMCD)
vacdaysRep <- rep(vaccdays,times = length(vaccines))
vaccinesRep <- rep(vaccines,each = length(vaccdays))

# read the FI file and mean it. it is made by script 2
fname <- 'outputs/limma_FI_EACHSUBJECT'
rnaFImeans <- calcFImean()

# read the original mean_on_day expressions and mean those
# this file needs to be copied into the folder. it is also used by scrip 1
fname <- 'outputs/limma'
rnameans <- calcFImean()

### AGILENT needs different probe numbers - needs fixing

