# --
# calculate metric 
# consumes observed and predicted values, ..., and decision boundary
# returns metric
# --
calc_metric <- function(observed, predicted, no_class, bound) {
  # TODO: add denom != 0 check
  cm <- calc_confusion_matrix(observed, predicted, no_class, bound)
  
  accuracy = (cm$TP + cm$TN) / (cm$TP + cm$TN + cm$FP + cm$FN)
  
  f1_score <- (2 * cm$TP) / ((2 * cm$TP) + cm$FP + cm$FN)
  
  recall <- cm$TP / (cm$TP + cm$FN)
  
  bounds <- seq(from = 0, to = 1, by = .0125)
  roc_auc <- calc_roc_auc(observed, predicted, no_class, bounds)
  
  result <- c(accuracy, f1_score, recall, roc_auc)
  
  return (result)
}

# --
# helper function to calculate confusion matrix
# consumers observed and predicted values, ..., and decision boundary
# returns list of TP, TN, FP, FN
# --
calc_confusion_matrix <- function(observed, predicted, no_class, bound) {
  pred_class <- if_else(predicted > bound, 1, no_class)
  TP <- sum(pred_class == 1 & observed == 1)
  TN <- sum(pred_class == no_class & observed == no_class)
  FP <- sum(pred_class == 1 & observed == no_class)
  FN <- sum(pred_class == no_class & observed == 1)
  
  return(list(TP = TP, TN = TN, FP = FP, FN = FN))
}

# --
# perform k-fold cross-validation
# consumes data, model type, model workflow, k, ..., and decision boundary
# returns metric average across k folds
# NOTE: in data, y must be the last column
# --
cross_validation <- function(data, model, model_wkflow, num_splits, no_class, bound) {
  set.seed(18938)
  df_cvs <- vfold_cv(data, v = num_splits)

  metrics_total <- 0
  
  for (i in 1:num_splits) {
    train_df <- analysis(df_cvs$splits[[i]])
    test_df  <- assessment(df_cvs$splits[[i]])
    
    model_fit <- model_wkflow |>
      fit(train_df) |>
      extract_fit_parsnip()
    
    if (model == "svc") {
      coefs <- tidy(model_fit)$estimate
      predictors <- cbind(test_df[-ncol(test_df)], INTERCEPT = 1)
      scores <- as.matrix(predictors) %*% coefs
      score1 <- max(scores)
      score0 <- min(scores)
      preds <- if_else(scores <= 0,
                       0.5 * ((scores - score0) / (-score0)),
                       0.5 + (0.5 * (scores / score1)))
    } else {
      preds <- predict(model_fit, new_data = test_df, type = "prob")$.pred_1
    }
    
    split_metrics <- calc_metric(test_df[[ncol(test_df)]], preds, no_class, bound)
    metrics_total = metrics_total + split_metrics
  }
  
  results <- metrics_total / num_splits
  df_results <- data.frame(metric = c("Accuracy",
                                      "F1 Score",
                                      "Recall",
                                      "ROC-AUC"),
                           value = results)
  
  return (df_results)
}

# --
# calculate ROC-AUC
# consumes observed and predicted values, ..., and list of boundaries
# --

# NOTE: this assumes bounds are valid for probabilities (between 0 and 1), 
# but that should be fine since we are defining it anyways

# TODO: this is very python-like right now, would like to convert to more
# r-oriented pattern (vectorized instead of iterating through lists)

calc_roc_auc <- function(observed, predicted, no_class, bounds) {
  # create vectors of senses, specs
  senses <- c()
  specs <- c()
  
  # for a bunch of boundaries: 
  for (i in 1:length(bounds)) {
    # calculate sens, specificity
    cm <- calc_confusion_matrix(observed, predicted, no_class, bounds[i])
    sens <- cm$TP / (cm$TP + cm$FN)
    spec <- cm$TN / (cm$TN + cm$FP)
    
    # add to vecs 
    senses <- c(senses, sens) 
    specs <- c(specs, spec)
  }
  
  # then, find the area under the curve (using trapezoidal area)
  area_sum <- 0
  for (i in 1:(length(senses) - 1)) {
    area <- ((senses[i] - senses[i+1]) * (specs[i] + specs[i + 1])) / 2
    area_sum <- area_sum + area
  }

  return(area_sum)
}