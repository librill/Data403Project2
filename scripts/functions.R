# --
# calculate metrics, perform cross validation
# --

# --
# calculate metric 
# consumes observed and predicted values, non-target class, and decision boundary
# returns metric
# --
calc_metrics <- function(observed, predicted, no_class, bound) {
  cm <- calc_confusion_matrix(observed, predicted, no_class, bound)

  accuracy = (cm$TP + cm$TN) / (cm$TP + cm$TN + cm$FP + cm$FN)
  f1_score <- (2 * cm$TP) / ((2 * cm$TP) + cm$FP + cm$FN)
  precision <- cm$TP / (cm$TP + cm$FP)
  recall <- cm$TP / (cm$TP + cm$FN)
  bounds <- seq(from = 0, to = 1, by = .00625)
  roc_auc <- calc_roc_auc(observed, predicted, no_class, bounds)
  
  result <- c(accuracy, f1_score, precision, recall, roc_auc)
  
  return (result)
}

# --
# helper function to calculate confusion matrix
# consumers observed and predicted values, non-target class, and decision boundary
# returns list of TP, TN, FP, FN
# --
calc_confusion_matrix <- function(observed, predicted, no_class, bound) {
  pred_class <- if_else(predicted > bound, 1, no_class)
  TP <- sum(pred_class == no_class & observed == no_class)
  TN <- sum(pred_class == 1 & observed == 1)
  FP <- sum(pred_class == no_class & observed == 1)
  FN <- sum(pred_class == 1 & observed == no_class)
  
  return(list(TP = TP, TN = TN, FP = FP, FN = FN))
}

# --
# returns single fold of original dataframe with predicted values, classifications
# consumes dataframe, model, worfklow, and makes predictions
# returns predictions combined with original dataframe to understand 
# --
understand_predictions <- function(data, model, model_wkflow, num_splits, bound) {
  set.seed(18938)
  df_cvs <- vfold_cv(data, v = num_splits)
  
  metrics_total <- 0
  
  train_df <- analysis(df_cvs$splits[[1]])
  test_df  <- assessment(df_cvs$splits[[1]])
  
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
  
  # get actual predictions and values 0/1
  test_df$preds <- round(preds, 4)
  test_df$vals <- ry <- ifelse(preds > bound, 1, 0)
  
  return(test_df)
}

# --
# perform k-fold cross-validation
# consumes data, model type, model workflow, k, non-target class, and decision boundary
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
    
    split_metrics <- calc_metrics(test_df[, ncol(test_df)], preds, no_class, bound)
    metrics_total = metrics_total + split_metrics
  }
  
  results <- metrics_total / num_splits
  df_results <- data.frame(metric = c("Accuracy",
                                      "F1 Score",
                                      "Precision",
                                      "Recall",
                                      "ROC-AUC"),
                           value = results)

  return (df_results)
}

# --
# calculate ROC-AUC
# consumes observed and predicted values, non-target class, and list of boundaries
# returns auc of roc
# --
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
  area <- sum((senses[-1] - senses[-length(senses)]) * (specs[-length(specs)] + specs[-1]) / 2)
  return(area)
}


# --
# calculate demographic parity, equal opportunity, and fpr parity
# consumes protected class (vector from data), observed and predicted values,
# non-target class, and a decision boundary
# returns dataframe of fairness metrics
# --
calc_fairness_metrics <- function(protected_class, observed, pred_class, no_class) {
  class_levels <- unique(protected_class)
  num_groups <- length(class_levels)
  
  # demographic parity
  positive_predictions <- c()
  # equal opportunity
  recalls <- c()
  # false positive rate parity
  error_rates <- c()
  
  for (i in 1:num_groups) {
    
    include <- (protected_class == class_levels[i])
    
    TP <- sum((pred_class[include] == no_class) & (observed[include] == no_class))
    TN <- sum((pred_class[include] == 1) & (observed[include] == 1))
    FP <- sum((pred_class[include] == no_class) & (observed[include] == 1))
    FN <- sum((pred_class[include] == 1) & (observed[include] == no_class))

    positive_predictions[i] <- (TP + FP) / (TP + TN + FP + FN)
    recalls[i] <- TP / (TP + FN)
    error_rates[i] <- FP / (TN + FP)
  }
  
  result <- data.frame(class_levels = class_levels,
                       demographic_parity = positive_predictions,
                       equal_opportunity = recalls,
                       fpr_parity = error_rates)
  
  return (result)
}