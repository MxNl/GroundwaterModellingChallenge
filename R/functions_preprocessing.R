slice_by_wellid <- function(x, nth = 1) {
  x |>
    group_by(well_id) |>
    group_split() |>
    magrittr::extract(nth) |>
      reduce(bind_rows)
}

slice_modelling_period <- function(x, split_dates) {
  x |> filter(between(date, split_dates$train_start, split_dates$train_end))
}

aggregated_predictors <- function(.data) {
  .data |>
    mutate(
      across(
        all_of(c("tn")),
        data.table::frollmean, n = 7, align = "right",
        .names = '{.col}_previous_week_mean'
      ),
      across(
        all_of(c("tn")),
        data.table::frollmean, n = 7*4, align = "right",
        .names = '{.col}_previous_month_mean'
      ),
      across(
        all_of(c("tn")),
        data.table::frollmean, n = 7*4*3, align = "right",
        .names = '{.col}_previous_quarteryear_mean'
      ),
      rr_previous_week_sum = data.table::frollsum(rr, n = 7, align = "right"),
      rr_previous_month_sum = data.table::frollsum(rr, n = 7 * 4, align = "right"),
      rr_previous_quarteryear_sum = data.table::frollsum(rr, n = 7 * 4 * 3, align = "right")
    )
}

add_lagged_predictors <- function(x, lags) {
  x |>
    timetk::tk_augment_lags(
      contains("previous_week"),
      .lags = lags
    )
}

aggregate_to_gwl_interval <- function(x) {
  x |>
    mutate(date = lubridate::ceiling_date(date, "week", week_start = -5, change_on_boundary = FALSE), .after = well_id) |>
    group_by(date) |>
    summarise(
      well_id = unique(well_id),
      across(c(everything(), -rr, -well_id), mean, na.rm = TRUE),
      across(rr, sum, na.rm = TRUE)
      # .groups = "drop"
    )
}
