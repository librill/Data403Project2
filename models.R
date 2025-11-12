library(tidyverse)
library(tidymodels)
library(discrim)
library(LiblineaR)
# for testing ground-truth ROC
# library(pROC)

source(here::here("data_cleaning.R"))
source(here::here("functions.R"))
source(here::here("splits.R"))

credit <- credit |> 
  na.omit()
# --
# LOGISTIC
# --

lg_rec_1 <- recipe(TARGET ~ ., data = credit) |>
  step_mutate(TARGET = factor(TARGET)) |>
  step_dummy(all_nominal_predictors())

logit_mod <- logistic_reg() |>
  set_mode("classification") |>
  set_engine("glm")

logit_wkflow <- workflow() |>
  add_model(logit_mod) |>
  add_recipe(lg_rec_1)

cross_validation(data = credit, model = "logistic", model_wkflow = logit_wkflow,
                 num_splits = 5, no_class = 0, bound = 0.5)

# Completely Random Sample:
splits <- random_split(credit, train_prop = 0.7, val_prop = 0.1, seed = 123)
train_random <- splits$train
val_random <- splits$validation
test_random <- splits$test

lg_rec_1 <- recipe(TARGET ~ ., data = train_random) |>
  step_mutate(TARGET = factor(TARGET)) |>
  step_naomit(all_predictors()) |>
  step_dummy(all_nominal_predictors())

logit_mod <- logistic_reg() |>
  set_mode("classification") |>
  set_engine("glm")

logit_wkflow <- workflow() |>
  add_model(logit_mod) |>
  add_recipe(lg_rec_1)

val_random <- val_random |>
   mutate(TARGET = factor(TARGET))

cross_validation(data = val_random, model = "logistic", model_wkflow = logit_wkflow, num_splits = 5,
                 no_class = 0, bound = 0.5)

set.seed(18938)
cvs <- vfold_cv(credit, v = 5)
logit_wkflow |>
  fit_resamples(resamples = cvs, metrics = metric_set(accuracy, f_meas, precision, recall, roc_auc)) |>
  collect_metrics()

# --
# LDA
# --

lda_rec_1 <- recipe(TARGET ~ ., data = credit) |>
  step_mutate(TARGET = factor(TARGET)) |>
  step_zv(all_predictors())

lda_mod <- discrim_linear() |>
  set_mode("classification") |>
  set_engine("MASS")

lda_wkflow <- workflow() |>
  add_model(lda_mod) |>
  add_recipe(lda_rec_1)

# TODO: fix collinearity for better scores

cross_validation(data = credit, model = "lda", model_wkflow = lda_wkflow,
                 num_splits = 5, no_class = 0, bound = 0.5)

set.seed(18938)
cvs <- vfold_cv(credit, v = 5)
lda_wkflow |>
  fit_resamples(resamples = cvs, metrics = metric_set(accuracy, recall, roc_auc)) |>
  collect_metrics()

# --
# SVC
# --

# NOTE: SVC only working on 10,000 rows ...
svc_credit <- credit[1:500, ] |>
  mutate(TARGET = if_else(TARGET == 0, -1, 1))

svc_rec_1 <- recipe(TARGET ~ ., data = svc_credit) |>
  step_mutate(TARGET = factor(TARGET)) |>
  step_normalize(all_numeric_predictors()) |>
  step_zv(all_predictors())

svc_mod <- svm_linear() |>
  set_mode("classification") |>
  set_engine("LiblineaR")

svc_wkflow <- workflow() |>
  add_model(svc_mod) |>
  add_recipe(svc_rec_1)

# new function
cross_validation(data = svc_credit, model = "svc", model_wkflow = svc_wkflow,
                 num_splits = 3, no_class = -1, bound = 0.5)

# existing function (for value comparison)
set.seed(18938)
cvs <- vfold_cv(svc_credit, v = 3)
svc_wkflow |>
  fit_resamples(resamples = cvs, metrics = metric_set(accuracy, f_meas, precision, recall)) |>
  collect_metrics()
