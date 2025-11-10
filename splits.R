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

stratified_split <- function(df) {
  n <- nrow(df)
  
  train_n <- floor(0.70 * n)
  val_n <- floor(0.10 * n)
  
  train_idx <- sample(seq_len(n), train_n)
  rest <- df[-train_idx, ]
  
  validation_idx <- sample(seq_len(nrow(rest)), val_n)
  
  train <- df[train_idx, ]
  val   <- rest[validation_idx, ]
  test  <- rest[-validation_idx, ]
  
  list(train=train, val=val, test=test)
}
