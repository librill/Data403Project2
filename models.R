library(tidyverse)
library(tidymodels)
library(discrim)
# check, which one of these two?:
library(LiblineaR)
library(kernlab)

source(here::here("data_cleaning.R"))
source(here::here("functions.R"))

credit <- credit |>
  na.omit()

lg_rec_1 <- recipe(TARGET ~ ., data = credit) |>
  step_mutate(TARGET = factor(TARGET)) |>
  step_dummy(all_nominal_predictors())

logit_mod <- logistic_reg() |>
  set_mode("classification") |>
  set_engine("glm")

logit_wkflow <- workflow() |>
  add_model(logit_mod) |>
  add_recipe(lg_rec_1)

cross_validation(data = credit, model = "logistic", model_wkflow = logit_wkflow, num_splits = 5,
                 metric = "accuracy", no_class = 0, bound = 0) 

cross_validation(data = credit, model = "logistic", model_wkflow = logit_wkflow, num_splits = 5,
                 metric = "recall", no_class = 0, bound = 0)

lda_rec_1 <- recipe(TARGET ~ ., data = credit) |>
  step_mutate(TARGET = factor(TARGET)) |>
  step_zv(all_predictors())

lda_mod <- discrim_linear() |>
  set_mode("classification") |>
  set_engine("MASS")

lda_wkflow <- workflow() |>
  add_model(lda_mod) |>
  add_recipe(lda_rec_1)

# TODO: fix collinearity
# TODO: create constants ...?

cross_validation(data = credit, model = "lda", model_wkflow = lda_wkflow, num_splits = 5,
                 metric = "accuracy", no_class = 0, bound = 0)

svc_rec_1 <- recipe(TARGET ~ ., data = credit) |>
  step_mutate(TARGET = factor(if_else(TARGET == 0, -1, TARGET))) |>
  step_zv(all_predictors())

svc_mod <- svm_linear() |>
  set_mode("classification") |>
  set_engine("kernlab", prob.model = TRUE) # check, not LiblineaR?

svc_wkflow <- workflow() |>
  add_model(svc_mod) |>
  add_recipe(svc_rec_1)

cross_validation(data = credit, model = "svc", model_wkflow = svc_wkflow, num_splits = 5,
                 metric = "accuracy", no_class = -1, bound = 0) # check, why error?
