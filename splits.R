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

stratified_split <- function(df, target, train_prop = 0.70, val_prop = 0.10, seed = 123) {
  set.seed(seed)
  yquo <- rlang::enquo(target)
  
  df %>%
    mutate(.target = as.factor(!!yquo)) %>%
    group_by(.target) %>%
    group_split() %>%
    lapply(function(d) {
      n <- nrow(d)
      n_train <- floor(train_prop * n)
      n_val   <- floor(val_prop   * n)
      idx <- sample.int(n)
      list(
        train = d[idx[1:n_train], ],
        val   = d[idx[(n_train + 1):(n_train + n_val)], ],
        test  = d[idx[(n_train + n_val + 1):n], ]
      )
    }) -> parts
  
  out <- list(
    train = bind_rows(lapply(parts, `[[`, "train")) %>% dplyr::select(-.target),
    val   = bind_rows(lapply(parts, `[[`, "val"))   %>% dplyr::select(-.target),
    test  = bind_rows(lapply(parts, `[[`, "test"))  %>% dplyr::select(-.target)
  )
  
  make_target_factor <- function(x) {
    x %>% mutate(TARGET = factor(as.character(TARGET), levels = c("1","0")))
  }
  
  out$train <- make_target_factor(out$train)
  out$val   <- make_target_factor(out$val)
  out$test  <- make_target_factor(out$test)
  out
}
