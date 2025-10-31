library(tidyverse)
library(tidymodels)

source(here::here("data_cleaning.R"))
source(here::here("functions.R"))

credit <- credit |>
  mutate(TARGET = factor(TARGET))

rec_1 <- recipe(TARGET ~ ., data = credit) |>
  step_dummy(all_nominal_predictors())

logit_mod <- logistic_reg() |>
  set_mode("classification") |>
  set_engine("glm")

logit_wkflow <- workflow() |>
  add_model(logit_mod) |>
  add_recipe(rec_1)

cross_validation(data = credit, model_wkflow = logit_wkflow, num_splits = 5,
                 metric = "recall", no_class = 0)
