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
  stopifnot(train_prop > 0, val_prop >= 0, train_prop + val_prop < 1)
  
  set.seed(seed)
  yquo <- enquo(target)
  yname <- as_name(yquo)
  
  parts <- df %>%
    mutate(.target = as.factor(!!yquo)) %>%
    group_by(.target) %>%
    group_split() %>%
    lapply(function(d) {
      n <- nrow(d)
      n_train <- floor(train_prop * n)
      n_val   <- floor(val_prop   * n)
      if (n >= 3) {
        n_train <- max(1, n_train)
        n_val   <- max(0, min(n - 2, n_val))
      }
      idx <- sample.int(n)
      list(
        train = d[idx[1:n_train], , drop = FALSE],
        val   = d[idx[(n_train + 1):(n_train + n_val)], , drop = FALSE],
        test  = d[idx[(n_train + n_val + 1):n], , drop = FALSE]
      )
    })
  
  bind_and_fix <- function(slot) {
    out <- bind_rows(lapply(parts, `[[`, slot)) %>% dplyr::select(-.target)
    out <- out %>%
      mutate(!!yname := factor(as.character(!!yquo), levels = c("0","1"))) %>%
      sample_frac(1) 
    out
  }
  
  list(
    train = bind_and_fix("train"),
    val   = bind_and_fix("val"),
    test  = bind_and_fix("test")
  )
}

