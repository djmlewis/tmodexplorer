# file paths must be localised

calcFI <- function() {
  rna <- read_rds(
    paste0("D2/outputs/", fname, ".rds"))
  # make a blank duplicate dataframe with NAs thatw e will use to complete the FI
  fi <- as.data.frame(matrix(NA, ncol = ncol(rna), nrow = nrow(rna)))
  names(fi) <- names(rna)
  fi$X1 <- rna$X1 # copy the RIDs from first column NB limma uses RID..1.1 wherease agilent uses 1,2,3

    # pass the subIDs to a walk
  walk(subids, function(id) {
    # extract the rows in the dm dataframe with our subject.
    print(paste0('Processing...',id))
    dm4Subject <- filter(dm, SUBJID == id) %>%
      arrange(VACCDY)
    # ColName has the column names for our subject - They are  sorted by VACCDY. 
    # check we have a day 0 it is the first row of extracted rows
    if (dm4Subject$VACCDY[1] == 0) {
      # we have a day 0 to use - No need to do anything if false as we have already filled with NAs
      # save the day 0 data for speed and readability
      day0ColName <- dm4Subject$ColName[[1]]
      day0ColData <- rna[[day0ColName]]
      # now walk down the column names for our SUBJID and replace the column with the divisor
      walk(dm4Subject$ColName, function(colN) {
        # <<- mods the outer variable.
        # rna has log2 expression values so we subtract logs to get a ratio
        fi[[colN]] <<- rna[[colN]] - day0ColData
      })
    } else {
      print(paste0('ERROR Processing...',id))
    }
  })
  
  # sort the col names by the subj-day order 
  fi <- fi %>%
    select(X1,sortedColNames)

  print ('Writing FI for each subject..')
  if(!dir.exists('D2/outputs')){dir.create('D2/outputs')}
  write_rds(fi, paste0("D2/outputs/",fname,"_FI_EACHSUBJECT.rds"))

}

####### MAKING THE RDS FILES FROM THE excel files
# uncomment and DO this ONCE only before running the script
# if(!dir.exists('D2/outputs')){dir.create('D2/outputs')}
# write_rds(read_csv("D2/normalized_expression_values_ghent_boostrix_limma.csv"),path = "D2/outputs/limma.rds")
# write_rds(read_csv("D2/normalized_expression_values_ghent_boostrix_agilent.csv"),path = "D2/outputs/agilent.rds")
# write_rds(read_csv("D2/targets_metadata_ghent_boostrix.csv"),path = "D2/outputs/targets.rds")
# write_rds(read_csv("D2/annotation_ghent_boostrix.csv"),path = "D2/outputs/annotation.rds")


################## SCRIPT
rm(list = setdiff(ls(), lsf.str()))

# load DM and filter out control wells
dm <- read_rds("D2/outputs/targets_DM.rds") %>%
  arrange(SUBJID,VACCDY)
sortedColNames <- dm$ColName

# extract the unique subject IDs
subids <- unique(dm$SUBJID)

# read the original data with each column as a probeset
# the raw data files named as below must be copied into the OUTPUTS folder.
# Rename the original MPIIB files as no script makes them
fname <- 'limma'
calcFI()

# # agilent uses a 1 2 3 4 system for probe ID unlike limma - NEEDS FIXING
# fname <- 'agilent'
# calcFI()

