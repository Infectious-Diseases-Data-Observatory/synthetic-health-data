library(synthpop)
library(tidyverse)
library(lme4)
library(ggfortify)
library(nlme)
library(gridExtra)
library(forecast)
library(ggplotify)
library(caret)
library(xgboost)

source("R/convert_to_numeric.R")
source("R/convert_to_factor.R")

source("R/syn_dm.R")

set.seed(1)

# For justification of methods, see the diss

# --- PREPROCESSING --- # --------------------------------------------------------------
# - DM Domain - DM_orig is participant data processed and stored by IDDO; not open source data since it contains personal information#
# - see IDDO website on how to access data #

# Selection of study 1 (PQVMQ) as in Methodology 4.1
DM_orig = read_csv("data/DM 2024-04-02.csv", guess_max = Inf, show_col_types = FALSE)

DM = syn_dm(DM_orig %>% 
         filter(STUDYID == "PQVMQ", SEX == "F"))

# LB pivoted
LB <- read.csv("data/LB 2024-04-02.csv") %>%  
  filter(LBTESTCD == "HGB",
         STUDYID == "PQVMQ") %>% 
  PREP_LB_FU() %>% 
  select(-c(VISITNUM, VISITDY, EMPTY_TIME)) %>% 
  arrange(DAY) %>% 
  pivot_wider(
    id_cols = c(STUDYID, USUBJID),
    names_from = DAY,
    values_from = c(HGB, HGB_UNITS),
    names_vary = "slowest"
  ) %>% 
  select(USUBJID, HGB_1, HGB_8, HGB_15, HGB_29, HGB_43, HGB_64) %>% # Make LBDY uniform, remove constant units
  arrange(USUBJID) %>% # Before joining, make sure correct each USUBJID has their data, line them up
  select(-USUBJID) # Then remove USUBJID, cbind, add back in randomly

DMLB <- cbind(DM, LB) %>%
  mutate(USUBJID = sample(1:3428)) %>% # Anonymise USUBJID, ensure shuffling
  relocate(USUBJID, .before = COUNTRY) %>%
  arrange(USUBJID) %>% 
  select(-STUDYID) # No need for this anymore

# Using function to convert classes
DMLB <- convert_to_numeric(DMLB, c("USUBJID", "RFSTDTC", "AGE", "HGB_1", "HGB_8", 
                                   "HGB_15", "HGB_29", "HGB_43", "HGB_64"))
DMLB <- convert_to_factor(DMLB, c("COUNTRY", "RFSTDTC", "SITEID", "SEX", "AGEU",
                                  "ARMCD"))

# --- SYNTHESIS --- # -------------------------------------------------------------------------------------------------
syn <- syn(DMLB, visit.sequence = 2:14, seed = 1, smoothing = "density", )

DMLB_syn <- syn$syn

compare(DMLB, DMLB_syn, nrow = 1, ncol = 2)
