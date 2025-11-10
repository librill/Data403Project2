# --
# calculate metric 
# consumes observed and predicted values, metric type, ..., and decision boundary
# returns metric
# --

# NOTE: metrics are opposite for logistic (but this is correct, would change 
# comparision for decision boundary)

calc_metric <- function(observed, predicted, metric, no_class, bound) {
  # TODO: convert this into another function ...
  pred_class <- if_else(predicted > bound, 1, no_class)
  TP <- sum(pred_class == 1 & observed == 1)
  TN <- sum(pred_class == no_class & observed == no_class)
  FP <- sum(pred_class == 1 & observed == no_class)
  FN <- sum(pred_class == no_class & observed == 1)
  
  if (metric == "roc_auc") {
    # TODO: create helper function to calculate roc-auc
  } else if (metric == "accuracy") {
    num <- TP + TN
    denom <- TP + TN + FP + FN
  } else if (metric == "f1") {
    num <- 2 * TP
    denom <- (2 * TP) + FP + FN
  } else if (metric == "recall") {
    num <- TP
    denom <- TP + FN
  } else {
    stop ("ERROR: not an available metric")
  }
  if (denom == 0) {
    stop ("ERROR: denominator is 0")
  }
  return (num / denom)
}

# --
# perform k-fold cross-validation
# consumes data, model type, model workflow, k, evaluation metric, ..., and
# decision boundary
# returns metric average across k folds
# --

# NOTE: in data, y must be the last column

cross_validation <- function(data, model, model_wkflow, num_splits, metric, no_class, bound) {
  set.seed(18938)
  df_cvs <- vfold_cv(data, v = num_splits)
  
  # TESTING TO MAKE SURE WE ARE GETTING CORRECT METRICS
  # fit <- model_wkflow |>
  #   fit(data) |>
  #   tidy()
  # fit_metrics <- model_wkflow |>
  #  fit_resamples(resamples = df_cvs, metrics = metric_set(accuracy, recall, precision)) |>
  #  collect_metrics()
  
  # print(fit_metrics)

  metric_total <- 0
  
  for (i in 1:num_splits) {
    train_df <- analysis(df_cvs$splits[[i]])
    test_df  <- assessment(df_cvs$splits[[i]])
    
    model_fit <- model_wkflow |>
      fit(train_df) |>
      extract_fit_parsnip()
    
    if (model == "logistic") {
      preds <- predict(model_fit, new_data = test_df, type = "raw")
    } else if (model == "lda") {
      preds <- predict(model_fit, new_data = test_df, type = "raw")$x
    } else {
      preds <- predict(model_fit, new_data = test_df, type = "prob")$.pred_1
    }
    
    split_metric <- calc_metric(test_df[[ncol(test_df)]], preds, metric, no_class, bound)
    metric_total = metric_total + split_metric
  }
  
  result <- metric_total / num_splits
  
  return (result)
}

# --
# calculate ROC-AUC
# consumes observed and predicted values, ..., and list of boundaries
# --

# TODO: this is going to assume that the list of bounds are valid, 
# which may or may not be good

# also not entirely sure how this is going to fit into function framework ..

# TODO: this is very python-like right now, would like to convert to more
# r-oriented pattern (vectorized instead of iterating through lists)

calc_roc_auc <- function(observed, predicted, no_class, bounds) {
  # create vectors of senses, specs
  senses <- c()
  specs <- c()
  
  # for a bunch of boundaries: 
  for (i in 1:len(bounds)) {
    # calculate sens, specificity
    pred_class <- if_else(predicted > bound, 1, no_class)
    TP <- sum(pred_class == 1 & observed == 1)
    TN <- sum(pred_class == no_class & observed == no_class)
    FP <- sum(pred_class == 1 & observed == no_class)
    FN <- sum(pred_class == no_class & observed == 1)
    sens <- TP / (TP + FN)
    spec <- TN / (TN + FP)
    
    # add to vecs 
    senses <- c(senses, sens) 
    specs <- c(specs, spec)
  }
  
  # now that we have vecs, plot?
  # TODO ...
  
  area_sum <- 0
  
  # then, find the area under the curve (using trapezoidal area)
  for (i in 1:len(senses) - 1) {
    area <- ((senses[i + 1] - senses[i]) * (specs[i + 1] + specs[i])) / 2
    area_sum <- area_sum + sum
  }
  
  return(area_sum)
}