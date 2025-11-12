# NOTE: this is for stratified, used in strat_split()
# install.packages("splitstackshape")
library(splitstackshape)

# TODO: make sure that we are using "train", "test", and "val" consistently
# NOTE: for basically all of the function, we are assuming 'TARGET' is the
# response variable and that it has values 0 and 1

# --
# split data randomly
# consumes dataframe, desired proportions of train, validation sets, seed
# returns randomly split train, validation, and test sets
# --
random_split <- function(df, train_prop = 0.7, val_prop = 0.1, seed = 123) {
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
# *new* stratified split
# consumes dataframe, name of column to be split on, training and validation
# set proportions, and seed (for reproducibility)
# returns stratified train, validation, and test sets
# NOTE: stratified() returns data.table, so need to convert to data.frame
# --
strat_split <- function(df, target, train_prop = 0.7, val_prop = 0.1, seed = 123) {
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
  
  # maybe have to randomize the order, as well? that could be an issue ...
  train_indices <- sample(nrow(train))
  val_indices <- sample(nrow(val))
  test_indices <- sample(nrow(test))
  
  train <- train[train_indices, ]
  val <- val[val_indices, ]
  test <- test[test_indices, ]
  
  return(list(train=train, val=val, test=test))
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
  
  # count how many postive, get correct proportion negative
  num_pos <- nrow(df_positive)
  num_neg <- floor(((1 - pct_positive) / pct_positive) * num_pos)
  
  # sample negative observations randomly
  df_negative_sampled <- df_negative[sample(nrow(df_negative), num_neg, replace=FALSE), ]
  
  # return combined dataframe
  all_sampled <- rbind(df_positive, df_negative_sampled)
  return(all_sampled)
}

# --
# *old* stratified split
# --
# stratified_split <- function(df, target, train_prop = 0.70, val_prop = 0.10, seed = 123) {
#   stopifnot(train_prop > 0, val_prop >= 0, train_prop + val_prop < 1)
#   
#   set.seed(seed)
#   yquo <- enquo(target)
#   yname <- as_name(yquo)
#   
#   parts <- df %>%
#     mutate(.target = as.factor(!!yquo)) %>%
#     group_by(.target) %>%
#     group_split() %>%
#     lapply(function(d) {
#       n <- nrow(d)
#       n_train <- floor(train_prop * n)
#       n_val   <- floor(val_prop   * n)
#       if (n >= 3) {
#         n_train <- max(1, n_train)
#         n_val   <- max(0, min(n - 2, n_val))
#       }
#       idx <- sample.int(n)
#       list(
#         train = d[idx[1:n_train], , drop = FALSE],
#         val   = d[idx[(n_train + 1):(n_train + n_val)], , drop = FALSE],
#         test  = d[idx[(n_train + n_val + 1):n], , drop = FALSE]
#       )
#     })
#   
#   bind_and_fix <- function(slot) {
#     out <- bind_rows(lapply(parts, `[[`, slot)) %>% dplyr::select(-.target)
#     out <- out %>%
#       mutate(!!yname := factor(as.character(!!yquo), levels = c("0","1"))) %>%
#       sample_frac(1) 
#     out
#   }
#   
#   list(
#     train = bind_and_fix("train"),
#     val   = bind_and_fix("val"),
#     test  = bind_and_fix("test")
#   )
# }



