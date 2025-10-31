calc_metric <- function(observed, predicted, metric, no_class) {
  
  pred_class <- if_else(predicted > 0, 1, no_class)
  TP <- sum(predicted == 1 & observed == "1")
  TN <- sum(predicted == no_class & observed == no_class)
  FP <- sum(predicted == 1 & observed == no_class)
  FN <- sum(predicted == no_class, observed == 1)
  
  print(TP)
  
  if (metric == "roc_auc") {
    # ***
  } else if (metric == "accuracy") {
    result <- (TP + TN) / (TP + TN + FP + TN)
  } else if (metric == "f1") {
    result <- (2 * TP) / ((2 * TP) + FP + FN)
  } else if (metric == "recall") {
    result <- TP / (TP + FN)
  } else {
    print("not an available metric")
    break
  }
  
  return (result)
}

# y must be the last column
cross_validation <- function(data, model_wkflow, num_splits, metric, no_class) {
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
    
    split_metric <- calc_metric(test_df[ncol(test_df)], preds, metric, no_class)
    
    print(split_metric)
    
    metric_total = metric_total + split_metric
  }
  
  result <- metric_total / num_splits
  
  return (result)
}