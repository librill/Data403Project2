# --
# calculate metric 
# consumes observed and predicted values, ..., and decision boundary
# returns metric
# --

# TODO: add denom != 0 check

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
# consumers observed and predicted values, ..., and decision boundary
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
# perform k-fold cross-validation
# consumes data, model type, model workflow, k, ..., and decision boundary
# returns metric average across k folds
# NOTE: in data, y must be the last column
# --

# --
# consumes dataframe, model, worfklow, and makes predictions
# returns predictions combined with original dataframe to understand 
# NOTE: just for one fold ...
# characteristics in FALSE POSITIVES (getting a lot of those right now ..)
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
  
  # get actual predictions, and values 0/1
  test_df$preds <- round(preds, 4)
  test_df$vals <- ry <- ifelse(preds > bound, 1, 0)
  
  return(test_df)
}

# TODO: how can we see the actual vs. predicted? could look at those in the 
# context of the data to make a decision about what we want to do moving forward

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
# consumes observed and predicted values, ..., and list of boundaries
# returns auc of roc
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


# --
# calculate demographic parity, equal opportunity, and fpr parity
# consumes protected class (vector from data), observed and predicted values, ..., and a decision boundary
# returns fairness metrics
# --
calc_fairness_metrics <- function(protected_class, observed, pred_class, no_class, bound) {
  class_levels <- unique(protected_class)
  num_groups <- length(class_levels)
  
  # demographic parity
  positive_predictions <- c()
  # equal opportunity
  recalls <- c()
  # false positive rate parity
  error_rates <- c()
  
  for (i in 1:num_groups) {
    include <- protected_class == class_levels[i]
    cm <- calc_confusion_matrix(observed[include], pred_class[include], no_class, bound)
    positive_predictions[i] <- (cm$TP + cm$FP) / (cm$TP + cm$TN + cm$FP + cm$FN)
    recalls[i] <- cm$TP / (cm$TP + cm$FN)
    error_rates[i] <- cm$FP / (cm$TN + cm$FP)
  }
  
  result <- data.frame(class_levels = class_levels,
                       demographic_parity = positive_predictions,
                       equal_opportunity = recalls,
                       fpr_parity = error_rates)
  
  return (result)
}
