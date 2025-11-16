# -- 
# load data, perform cleaning
# --

# --
# credit data
# --
all_credit <- read_csv(here::here("Data", "application_train.csv"))

# to create income categories
bottom <- 100000
middle <- 150000
top <- 250000

credit <- all_credit %>%
  mutate(TARGET = factor(TARGET, levels = c(0, 1))) %>%
  mutate(
    DOCUMENTS_FLAGGED = rowSums(dplyr::select(., starts_with("FLAG_DOCUMENT"))),
    DAYS_BIRTH = abs(DAYS_BIRTH)
  ) %>%
  mutate(
    CONTRACT_TYPE_CASH = ifelse(NAME_CONTRACT_TYPE=="Cash loans", 1, 0),
    CONTRACT_TYPE_REVOLVING = ifelse(NAME_CONTRACT_TYPE=="Revolving loans", 1, 0),
    EDUCATION_TYPE_SECONDARY = ifelse(NAME_EDUCATION_TYPE=="Secondary / secondary special", 1, 0),
    EDUCATION_TYPE_HIGHER = ifelse(NAME_EDUCATION_TYPE=="Higher education", 1, 0),
    FAMILY_STATUS_MARRIED = ifelse(NAME_FAMILY_STATUS=="Married", 1, 0),
    FAMILY_STATUS_SINGLE = ifelse(!NAME_FAMILY_STATUS == "Single / not married", 1, 0),
    HOUSING_TYPE_OWN = ifelse(NAME_HOUSING_TYPE=="House / apartment", 1, 0),
    HOUSING_TYPE_PARENTS = ifelse(NAME_HOUSING_TYPE=="With parents", 1, 0),
    FLAG_OWN_CAR = ifelse(FLAG_OWN_CAR =="Y", 1, 0),
    FLAG_OWN_REALTY = ifelse(FLAG_OWN_REALTY=="Y", 1, 0),
    AGE = DAYS_BIRTH / 365,
    AGE_GROUPS = case_when(AGE < 35 ~ "<35",
                           AGE < 45 ~ "35-45",
                           AGE < 55 ~ "45-55",
                           AGE >= 55 ~ "55+"),
    CODE_GENDER = ifelse(CODE_GENDER=="F", 1, 0)
  ) %>%
  mutate(
    INCOME_CATEGORY = case_when(
      AMT_INCOME_TOTAL < bottom ~ "FOURTH", 
      AMT_INCOME_TOTAL > bottom & AMT_INCOME_TOTAL <= middle ~ "THIRD", 
      AMT_INCOME_TOTAL > middle & AMT_INCOME_TOTAL <= top ~ "SECOND", 
      AMT_INCOME_TOTAL > top ~ "FIRST"
    )
  ) %>% 
  mutate(
    INCOME_CLASS_FOURTH = ifelse(INCOME_CATEGORY == "FOURTH", 1, 0), 
    INCOME_CLASS_THIRD = ifelse(INCOME_CATEGORY == "THIRD", 1, 0),
    INCOME_CLASS_SECOND = ifelse(INCOME_CATEGORY == "SECOND", 1, 0),
    INCOME_CLASS_FIRST = ifelse(INCOME_CATEGORY == "FIRST", 1, 0)
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
    FAMILY_STATUS_SINGLE,
    HOUSING_TYPE_OWN,
    HOUSING_TYPE_PARENTS,
    DAYS_EMPLOYED,
    DOCUMENTS_FLAGGED,
    INCOME_CLASS_FOURTH,
    INCOME_CLASS_THIRD, 
    INCOME_CLASS_SECOND,
    REGION_POPULATION_RELATIVE,
    CODE_GENDER,
    AGE_GROUPS,
    TARGET
  )

credit <- credit |>
  na.omit()

# --
# bureau data
# --
all_bureau <- read_csv(here::here("Data", "bureau.csv"))

bureau_grouped <- all_bureau %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    NUM_CREDITS = n(), 
    MEAN_DAYS_CREDIT = round(mean(abs(DAYS_CREDIT), na.rm = TRUE), 2)
  )

bureau_grouped <- bureau_grouped |>
  na.omit()

# --
# now, group both of them
# --
the_merged <- left_join(credit, bureau_grouped, by="SK_ID_CURR") %>%
  mutate(
    NUM_CREDITS = if_else(is.na(NUM_CREDITS), 0, NUM_CREDITS),
    MEAN_DAYS_CREDIT = if_else(is.na(MEAN_DAYS_CREDIT), 0, MEAN_DAYS_CREDIT),
    INCOME_TO_CREDIT = round(credit$AMT_INCOME_TOTAL / credit$AMT_CREDIT, 2),
    CREDIT_TO_NUM_CREDITS = round(NUM_CREDITS / credit$AMT_CREDIT, 2), 
    INCOME_TO_REGION = round(credit$AMT_INCOME_TOTAL / credit$REGION_POPULATION_RELATIVE, 2)
  )

# move 'TARGET' back to the last column
all_cols <- names(the_merged)
remaining_cols <- all_cols[all_cols != "TARGET"]
new_order <- c(remaining_cols, "TARGET")

# apply the new order to the data frame
df <- the_merged[, new_order]
