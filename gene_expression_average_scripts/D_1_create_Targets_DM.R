rm(list = setdiff(ls(), lsf.str()))

# file paths will have to be localised
targets_DM<- read_csv("targets_metadata_ghent_1_v3.csv",
                col_types = cols(
                  X1 = col_character(),
                  FileName = col_character(),
                  donor = col_integer(),
                  timepoint = col_character(),
                  sample_ID = col_character(),
                  study_ID = col_character(),
                  age = col_integer(),
                  sex = col_character(),
                  vaccine = col_character(),
                  dn = col_integer(),
                  class = col_character()
                )) %>%
  rename(SUBJID = donor, VACCDY = dn, ColName = X1) %>%
  filter(is.na(VACCDY) == FALSE) %>%
  full_join(read_csv("CRC305D_DM.csv",
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

if(!dir.exists('D1_v3/outputs')){dir.create('D1_v3/outputs')}
write_rds(targets_DM,"D1_v3/outputs/targets_DM.rds")