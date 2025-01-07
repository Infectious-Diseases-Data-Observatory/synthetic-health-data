library(synthpop)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lme4)
library(synthpop)
library(ggfortify)
library(nlme)
library(gridExtra)
library(forecast)
library(ggplotify)
library(caret)
library(xgboost)
#setwd("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code")
set.seed(1)

# For justification of methods, see the diss

# --- PREPROCESSING --- # --------------------------------------------------------------
# - DM Domain - DM_orig is participant data processed and stored by IDDO; not open source data since it contains personal information#
# - see IDDO website on how to access data #

DM_orig <- read.csv("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Data/DM 2024-04-02.csv")
DM <- DM_orig

# Selection of study 1 (PQVMQ) as in Methodology 4.1
DM <- DM %>% 
  filter (STUDYID == "PQVMQ") %>%
  select(-DTHFL, -RACE, -ETHNIC, -DMDY, -ARM, -DOMAIN) %>% # Remove blanks, duplicates, and constant variables
  mutate(RFSTDTC = str_extract(RFSTDTC, "[0-9]{1,4}")) %>% # Remove months from RFSTDTC
  relocate(COUNTRY, .after = STUDYID) %>%
  relocate(AGE, .after = SEX) %>% 
  relocate(AGEU, .after = SEX) %>% 
  arrange(USUBJID) %>% # Before joining, make sure correct each USUBJID has their data, line them up
  select(-USUBJID) # Then remove USUBJID, cbind, add back in randomly

# - LB - #
LB_orig <- read.csv("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Data/LB 2024-04-02.csv")
# LB pivoted
LB <- read.csv("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Data/wider_by_LBDY.csv")

# Make LBDY uniform, remove constant units
LB <- LB %>% 
  select(USUBJID, HGB_1, HGB_8, HGB_15, HGB_29, HGB_43, HGB_64) %>% 
  arrange(USUBJID) %>% # Before joining, make sure correct each USUBJID has their data, line them up
  select(-USUBJID) # Then remove USUBJID, cbind, add back in randomly

DMLB <- cbind(DM, LB) %>%
  mutate(USUBJID = sample(1:3428)) %>% # Anonymise USUBJID, ensure shuffling
  relocate(USUBJID, .before = COUNTRY) %>%
  arrange(USUBJID) %>% 
  select(-STUDYID) # No need for this anymore

#---------------------------------------------------------------------------------------------------------------
# Functions to convert classes (just playing around)
convert_to_factor <- function(df, columns) {
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.factor(df[[col]])
    } else {
      warning(paste("Column", col, "not found in the dataframe."))
    }
  }
  return(df)
}

convert_to_numeric <- function(df, columns) {
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    } else {
      warning(paste("Column", col, "not found in the dataframe."))
    }
  }
  return(df)
}

DMLB %>% sapply(class)

# Using function to convert classes
DMLB <- convert_to_numeric(DMLB, c("USUBJID", "RFSTDTC", "AGE", "HGB_1", "HGB_8", 
                                   "HGB_15", "HGB_29", "HGB_43", "HGB_64"))
DMLB <- convert_to_factor(DMLB, c("COUNTRY", "RFSTDTC", "SITEID", "SEX", "AGEU",
                                  "ARMCD"))

DMLB %>% sapply(class)

# --- SYNTHESIS --- # -------------------------------------------------------------------------------------------------
DMLB_syn <- DMLB

syn <- syn(DMLB, visit.sequence = 2:14, seed = 1, smoothing = "density", )

DMLB_syn <- syn$syn

compare(DMLB, DMLB_syn, nrow = 1, ncol = 2)