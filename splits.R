# --
# different ways to split data
# --

# --
# split data randomly
# consumes dataframe, desired proportions of train, validation sets, seed
# returns randomly split train, validation, and test sets
# --
random_split <- function(df, train_prop = 0.6, val_prop = 0.2, seed = 123) {
  set.seed(seed)
  
  df_shuffled <- df |> slice_sample(prop = 1)
  n <- nrow(df_shuffled)
  
  train_n <- floor(train_prop * n)
  val_n   <- floor(val_prop * n)
  
  train <- df_shuffled[1:train_n, ]
  val   <- df_shuffled[(train_n + 1):(train_n + val_n), ]
  test  <- df_shuffled[(train_n + val_n + 1):n, ]
  
  list(train = train, validation = val, test = test)
}

# --
# stratified data split
# consumes dataframe, name of column to be split on, training and validation
# set proportions, and seed (for reproducibility)
# returns stratified train, validation, and test sets
# NOTE: stratified() returns data.table, so need to convert to data.frame
# --
strat_split <- function(df, target, train_prop = 0.6, val_prop = 0.2, seed = 123) {
  set.seed(seed)
  
  # split the data into two sets: training and the rest
  split_one = stratified(indt=df, group=target, size=train_prop, bothSets=TRUE)
  train <- as.data.frame(split_one$SAMP1)
  rest <- as.data.frame(split_one$SAMP2)
  
  # calculate validation set proportionally to the rest
  new_val_prop <- val_prop / (1 - train_prop)
  
  split_two = stratified(indt=rest, group=target, size=new_val_prop, bothSets=TRUE)
  val <- as.data.frame(split_two$SAMP1)
  test <- as.data.frame(split_two$SAMP2)
  
  # randomize order
  train_indices <- sample(nrow(train))
  val_indices <- sample(nrow(val))
  test_indices <- sample(nrow(test))
  
  train <- train[train_indices, ]
  val <- val[val_indices, ]
  test <- test[test_indices, ]
  
  return(list(train=train, val=val, test=test))
}

# ---
# non-random split 1: train on older loan -> test on newer loan applicants
# consumes dataframe, training and validation set proportions
# returns training, validation, and test sets
# ---
time_split <- function(df, train_prop = 0.60, val_prop = 0.20, seed = 123) {
  set.seed(seed)
  df_ord <- df %>% arrange(SK_ID_CURR)
  n <- nrow(df_ord)
  train_end <- floor(train_prop * n)
  val_end   <- floor((train_prop + val_prop) * n)
  
  list(
    train = df_ord[1:train_end, ],
    val   = df_ord[(train_end + 1):val_end, ],
    test  = df_ord[(val_end + 1):n, ]
  )
}

# --
# non-random split 2: split on incomes; train on medium and high income, test on low-income
# --
income_split <- function(df, val_prop_within_train = 0.20, seed = 123) {
  set.seed(seed)
  q <- quantile(df$AMT_INCOME_TOTAL, probs = c(1/3, 2/3), na.rm = TRUE)
  low  <- df %>% filter(AMT_INCOME_TOTAL <  q[1])
  mid  <- df %>% filter(AMT_INCOME_TOTAL >= q[1], AMT_INCOME_TOTAL <  q[2])
  high <- df %>% filter(AMT_INCOME_TOTAL >= q[2])
  
  trainval <- bind_rows(mid, high) %>% slice_sample(prop = 1)
  n_tv <- nrow(trainval)
  val_n <- floor(val_prop_within_train * n_tv)
  
  val_idx <- sample(seq_len(n_tv), size = val_n)
  val  <- trainval[val_idx, ]
  train <- trainval[-val_idx, ]
  
  test <- low %>% slice_sample(prop = 1)
  
  list(train = train, val = val, test = test)
}

# ---
# non-random split 3: split on employment type; train on "old" employment types and 
# test on "newer" employment types
# ---
loan_type_split <- function(df, val_prop_within_train = 0.20, use_cash_for_train = TRUE, seed = 123) {
  set.seed(seed)
  cash      <- df %>% filter(CONTRACT_TYPE_CASH == 1)
  revolving <- df %>% filter(CONTRACT_TYPE_REVOLVING == 1)
  
  if (use_cash_for_train) {
    trainval <- cash %>% slice_sample(prop = 1)
    test     <- revolving %>% slice_sample(prop = 1)
  } else {
    trainval <- revolving %>% slice_sample(prop = 1)
    test     <- cash %>% slice_sample(prop = 1)
  }
  
  n_tv <- nrow(trainval)
  val_n <- floor(val_prop_within_train * n_tv)
  val_idx <- sample(seq_len(n_tv), size = val_n)
  
  val   <- trainval[val_idx, ]
  train <- trainval[-val_idx, ]
  
  list(train = train, val = val, test = test)
}

# ---
# creates undersampled dataframe
# consumes dataframe, desired percent of positive class (1, in this case)
# returns new dataframe with correct proportion of positive and negative classes
# ---
undersample_split <- function(df, pct_positive = .5) {
  # want to grab all positive, negative classes
  df_positive <- df %>%
    filter(TARGET == 1)
  
  df_negative <- df %>%
    filter(TARGET == 0)
  
  # count how many positive, get correct proportion negative
  num_pos <- nrow(df_positive)
  num_neg <- floor(((1 - pct_positive) / pct_positive) * num_pos)
  
  # sample negative observations randomly
  df_negative_sampled <- df_negative[sample(nrow(df_negative), num_neg, replace=FALSE), ]
  
  # return combined dataframe
  all_sampled <- rbind(df_positive, df_negative_sampled)
  return(all_sampled)
}
