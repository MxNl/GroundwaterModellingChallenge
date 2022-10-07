targets_preprocessing <- list(
  tar_target(
    data_no_empty_cols,
    data %>%
      janitor::remove_empty("cols"),
    pattern = map(data),
    iteration = "list"
  ),
  tar_target(
    data_modelling,
    data_no_empty_cols %>%
      filter(date >= split_dates %>%
        pull(train_start) %>%
        lubridate::as_date()) %>%
      filter(date <= split_dates %>%
        pull(train_end) %>%
        lubridate::as_date()),
    pattern = map(data_no_empty_cols, split_dates),
    iteration = "list"
  ),
  tar_target(
    data_prediction,
    data_no_empty_cols %>%
      filter(date > split_dates %>%
        pull(train_end) %>%
        lubridate::as_date()),
    pattern = map(data_no_empty_cols, split_dates),
    iteration = "list"
  )
)