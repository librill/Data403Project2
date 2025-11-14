library(tidyverse)
library(dplyr)

# load dataframe
all_credit <- read_csv(here::here("Data", "application_train.csv"))

# option 1
# aggregate documents flagged, one-hot-encode, and select columns
# NOTE: some parts commented out to prevent collinearity
credit <- all_credit %>%
  mutate(TARGET = factor(TARGET, levels = c(0, 1))) %>%
  mutate(
    DOCUMENTS_FLAGGED = rowSums(dplyr::select(., starts_with("FLAG_DOCUMENT")))
  ) %>%
  mutate(
    CONTRACT_TYPE_CASH = ifelse(NAME_CONTRACT_TYPE=="Cash loans", 1, 0),
    CONTRACT_TYPE_REVOLVING = ifelse(NAME_CONTRACT_TYPE=="Revolving loans", 1, 0),
    EDUCATION_TYPE_SECONDARY = ifelse(NAME_EDUCATION_TYPE=="Secondary / secondary special", 1, 0),
    EDUCATION_TYPE_HIGHER = ifelse(NAME_EDUCATION_TYPE=="Higher education", 1, 0),
    FAMILY_STATUS_MARRIED = ifelse(NAME_FAMILY_STATUS=="Married", 1, 0),
    FAMILY_STATUS_OTHER = ifelse(!NAME_FAMILY_STATUS %in% c("Married", "Single / not married"), 1, 0),
    HOUSING_TYPE_OWN = ifelse(NAME_HOUSING_TYPE=="House / apartment", 1, 0),
    HOUSING_TYPE_PARENTS = ifelse(NAME_HOUSING_TYPE=="With parents", 1, 0),
    FLAG_OWN_CAR = ifelse(FLAG_OWN_CAR =="Y", 1, 0),
    FLAG_OWN_REALTY = ifelse(FLAG_OWN_REALTY=="Y", 1, 0)
  ) %>%
  mutate (
    INCOME_TO_CREDIT = round(AMT_INCOME_TOTAL / AMT_CREDIT, 2),
    INCOME_TO_REGION = round(AMT_INCOME_TOTAL / REGION_POPULATION_RELATIVE, 2)
  ) %>% 
  dplyr::select(
    SK_ID_CURR,
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
    FAMILY_STATUS_MARRIED,
    FAMILY_STATUS_OTHER,
    HOUSING_TYPE_OWN,
    HOUSING_TYPE_PARENTS,
    DAYS_EMPLOYED,
    DOCUMENTS_FLAGGED,
    REGION_POPULATION_RELATIVE,
    INCOME_TO_CREDIT,
    INCOME_TO_REGION,
    TARGET
  )



