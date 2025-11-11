library(tidyverse)
library(dplyr)

# load dataframe
all_credit <- read_csv(here::here("Data", "application_train.csv"))

# option 1
# aggregate documents flagged, one-hot-encode, and select columns
# NOTE: some parts commented out to prevent collinearity
credit <- all_credit %>% 
  mutate(TARGET = factor(TARGET, levels = c(0, 1)))
  mutate(
    DOCUMENTS_FLAGGED = rowSums(select(., starts_with("FLAG_DOCUMENT")))
  ) %>% 
  mutate(
    CONTRACT_TYPE_CASH = ifelse(NAME_CONTRACT_TYPE=="Cash loans", 1, 0), 
    CONTRACT_TYPE_REVOLVING = ifelse(NAME_CONTRACT_TYPE=="Revolving loans", 1, 0),
    EDUCATION_TYPE_SECONDARY = ifelse(NAME_EDUCATION_TYPE=="Secondary / secondary special", 1, 0),
    EDUCATION_TYPE_HIGHER = ifelse(NAME_EDUCATION_TYPE=="Higher education", 1, 0), 
    # EDUCATION_TYPE_OTHER = ifelse(!NAME_EDUCATION_TYPE %in% c("Secondary / secondary special", "Higher education"), 1, 0),
    FAMILY_STATUS_MARRIED = ifelse(NAME_FAMILY_STATUS=="Married", 1, 0), 
    # FAMILY_STATUS_SINGLE = ifelse(NAME_FAMILY_STATUS=="Single / not married", 1, 0), 
    FAMILY_STATUS_OTHER = ifelse(!NAME_FAMILY_STATUS %in% c("Married", "Single / not married"), 1, 0),
    HOUSING_TYPE_OWN = ifelse(NAME_HOUSING_TYPE=="House / apartment", 1, 0), 
    HOUSING_TYPE_PARENTS = ifelse(NAME_HOUSING_TYPE=="With parents", 1, 0), 
    # HOUSING_TYPE_OTHER = ifelse(!NAME_HOUSING_TYPE %in% c("House / apartment", "With parents"), 1, 0), 
    FLAG_OWN_CAR = ifelse(FLAG_OWN_CAR =="Y", 1, 0),
    FLAG_OWN_REALTY = ifelse(FLAG_OWN_REALTY=="Y", 1, 0)
  ) %>%
  select (
    SK_ID_CURR,
    # CONTRACT_TYPE_CASH, 
    CONTRACT_TYPE_REVOLVING, 
    FLAG_OWN_CAR,
    FLAG_OWN_REALTY,
    CNT_CHILDREN,
    AMT_INCOME_TOTAL,
    AMT_CREDIT,
    AMT_ANNUITY,
    AMT_GOODS_PRICE,
    EDUCATION_TYPE_SECONDARY, 
    EDUCATION_TYPE_HIGHER, 
    # EDUCATION_TYPE_OTHER, 
    FAMILY_STATUS_MARRIED, 
    # FAMILY_STATUS_SINGLE, 
    FAMILY_STATUS_OTHER, 
    HOUSING_TYPE_OWN, 
    HOUSING_TYPE_PARENTS, 
    # HOUSING_TYPE_OTHER,
    DAYS_EMPLOYED,
    DOCUMENTS_FLAGGED, 
    TARGET
  )

