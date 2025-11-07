calc_metric <- function(observed, predicted, metric, no_class, bound) {
  
  pred_class <- if_else(predicted > bound, 1, no_class)
  #observed <- as.numeric(as.character(observed))
  
  TP <- sum(pred_class == 1 & observed == 1)
  TN <- sum(pred_class == no_class & observed == no_class)
  FP <- sum(pred_class == 1 & observed == no_class)
  FN <- sum(pred_class == no_class & observed == 1)
  
  if (metric == "roc_auc") {
    # ***
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
    return ("not an available metric")
  }
  
  if (denom == 0) {
    return ("denominator is 0")
  }
  
  return (num / denom)
}

# y must be the last column
cross_validation <- function(data, model_wkflow, num_splits, metric, no_class, bound) {
  set.seed(18938)
  df_cvs <- vfold_cv(data, v = num_splits)
  
  metric_total <- 0
  
  for (i in 1:num_splits) {
    train_df <- analysis(df_cvs$splits[[i]])
    test_df  <- assessment(df_cvs$splits[[i]])
    
    model_fit <- model_wkflow |>
      fit(train_df) |>
      extract_fit_parsnip()
    
    preds <- predict(model_fit, new_data = test_df, type = "prob")$.pred_1
    
    split_metric <- calc_metric(test_df[[ncol(test_df)]], preds, metric, no_class, bound)
    
    metric_total = metric_total + split_metric
  }
  
  result <- metric_total / num_splits
  
  return (result)
}