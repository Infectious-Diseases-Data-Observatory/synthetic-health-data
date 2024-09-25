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
setwd("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code")
set.seed(1)



# For justification of methods, see the diss



# --- PREPROCESSING --- #



# - DM - #


DM_orig <- read.csv("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Data/DM 2024-04-02.csv")
DM <- DM_orig


# Selection of study 1 (PQVMQ) as in Methodology 4.1
DM <- DM %>% filter (STUDYID == "PQVMQ")

# Remove blanks, duplicates, and constant variables
DM <- DM %>% select(-DTHFL, -RACE, -ETHNIC, -DMDY, -ARM, -DOMAIN)

# Remove months from RFSTDTC
DM <- DM %>% mutate(RFSTDTC = str_extract(RFSTDTC, "[0-9]{1,4}"))

# Reordering
DM <- DM %>% relocate(COUNTRY, .after = STUDYID)
DM <- DM %>% relocate(AGE, .after = SEX)
DM <- DM %>% relocate(AGEU, .after = SEX)


# - LB - #


# LB data
LB_orig <- read.csv("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Data/LB 2024-04-02.csv")
# LB pivoted
LB <- read.csv("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Data/wider_by_LBDY.csv")


# Make LBDY uniform, remove constant units
LB <- LB %>% select(USUBJID,
                    HGB_1, HGB_8, HGB_15, 
                    HGB_29, HGB_43, HGB_64)

# Before joining, make sure correct each USUBJID has their data, line them up
DM <- DM %>% arrange(USUBJID)
LB <- LB %>% arrange(USUBJID)

# Then remove USUBJID, cbind, add back in randomly
DM <- DM %>% select(-USUBJID)
LB <- LB %>% select(-USUBJID)
DMLB <- cbind(DM, LB)

# Anonymise USUBJID, ensure shuffling
USUBJID <- sample(1:3428)
DMLB$USUBJID <- USUBJID
DMLB <- DMLB %>% relocate(USUBJID, .before = COUNTRY)
DMLB <- DMLB %>% arrange(USUBJID)

# No need for this anymore
DMLB <- DMLB %>% select(-STUDYID)



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



# --- SYNTHESIS --- #



DMLB_syn <- DMLB
syn <- syn(DMLB, visit.sequence = 2:14, seed = 1, 
                smoothing = "density", )
DMLB_syn <- syn$syn


compare(DMLB, DMLB_syn, nrow = 1, ncol = 2)



