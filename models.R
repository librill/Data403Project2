library(tidyverse)
library(tidymodels)
library(discrim)
library(kernlab)
# for testing ground-truth ROC
library(pROC) 

source(here::here("data_cleaning.R"))
source(here::here("functions.R"))

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

cross_validation(data = credit, model = "logistic", model_wkflow = logit_wkflow, num_splits = 5,
                 no_class = 0, bound = .5)

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

cross_validation(data = credit, model = "lda", model_wkflow = lda_wkflow, num_splits = 5,
                 no_class = 0, bound = .5)

# --
# SVC
# --

# NOTE: SVC only working on 10,000 rows ...
sub_credit <- credit[1:10000, ]

svc_rec_1 <- recipe(TARGET ~ ., data = sub_credit) |>
  step_mutate(TARGET = factor(if_else(TARGET == 0, -1, TARGET))) |>
  step_normalize(all_numeric_predictors()) |>
  step_zv(all_predictors())

# svc_mod <- svm_linear() |>
#   set_mode("classification") |>
#   set_engine("kernlab", prob.model = TRUE, fit.args = list(control = list(maxiter = 1e5)))

svc_mod <- svm_linear() |>
  set_mode("classification") |>
  set_engine("kernlab", max_iter = 1e100)

svc_wkflow <- workflow() |>
  add_model(svc_mod) |>
  add_recipe(svc_rec_1)

cross_validation(data = sub_credit, model = "svc", model_wkflow = svc_wkflow,
                 num_splits = 5, metric = "accuracy", no_class = 0, bound = 0.5)

set.seed(18938)
cvs <- vfold_cv(sub_credit, v = 5)
svc_wkflow |>
  fit_resamples(resamples = cvs, metrics = metric_set(accuracy, roc_auc)) |>
  collect_metrics()
