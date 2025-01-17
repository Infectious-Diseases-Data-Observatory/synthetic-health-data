syn_dm = function(data_dm){
  data_dm = data_dm %>%
    select(-DTHFL, -RACE, -ETHNIC, -DMDY, -ARM, -DOMAIN) %>% # Remove blanks, duplicates, and constant variables
    mutate(RFSTDTC = str_extract(RFSTDTC, "[0-9]{1,4}")) %>% # Remove months from RFSTDTC
    relocate(COUNTRY, .after = STUDYID) %>%
    relocate(AGE, .after = SEX) %>% 
    relocate(AGEU, .after = SEX) %>% 
    arrange(USUBJID) %>% # Before joining, make sure correct each USUBJID has their data, line them up
    select(-USUBJID)
  
  syn_dm = syn(data_dm, 
               visit.sequence = 2:7, 
               seed = 1, 
               smoothing = "density")
  
  return(syn_dm)
}

# syn_dm(DM %>% 
#          filter(STUDYID == "PQVMQ"))
