rm(list = setdiff(ls(), lsf.str()))

# file paths must be localised
targets_DM<- read_csv("targets_metadata_ghent_boostrix.csv",
                col_types = cols(
                  X1 = col_character(),
                  FileName = col_character(),
                  sample_ID = col_character(),
                  donor = col_integer(),
                  visit = col_character(),
                  timepoint = col_character(),
                  study_ID = col_character(),
                  age = col_character(),
                  sex = col_character(),
                  vaccine = col_character(),
                  hyb_order = col_integer(),
                  hyb_date = col_character(),
                  hyb_month = col_character(),
                  feature_extract = col_character(),
                  array_batch = col_integer()
                )) %>%
  select(SUBJID = donor, VACCDY = timepoint, ColName = X1) %>%
  filter(is.na(VACCDY) == FALSE) %>%
  # unlike D1 where dn was an integer timepoint is "D01" so we have to sub out the D and convert to integer
  mutate(VACCDY = as.integer(sub('D','',VACCDY))) %>%
  full_join(read_csv("CRC305D2_DM.csv",
               col_types = cols(
                 .default = col_character(),
                 SUBJID = col_integer(),
                 RFSTDTC = col_date(format = ""),
                 RFENDTC = col_date(format = ""),
                 RFXSTDTC = col_datetime(format = ""),
                 RFXENDTC = col_datetime(format = ""),
                 RFICDTC = col_datetime(format = ""),
                 RFPENDTC = col_date(format = ""),
                 AGE = col_integer()
               )), 
            by = 'SUBJID') %>%
  filter(is.na(ColName) == FALSE)

if(!dir.exists('D2/outputs')){dir.create('D2/outputs')}
write_rds(targets_DM,"D2/outputs/targets_DM.rds")