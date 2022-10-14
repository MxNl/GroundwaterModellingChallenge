make_resampling <- function(x) {
  x |> 
    timetk::time_series_cv(
      date,
      initial = CV_INITIAL,
      assess = CV_ASSESS,
      skip = CV_ASSESS,
      lag = CV_LAG,
      cumulative = TRUE
    )
}

make_recipe <- function(x) {
  well_id <- x |> 
    slice(1) |> 
    pull(well_id)
  
  locale_set <- case_when(
    well_id == "Germany" ~ "DE",
    TRUE ~ "World"
  )
  
  recipe(gwl ~ ., data = x) |> 
    timetk::step_timeseries_signature(date) |> 
    timetk::step_holiday_signature(date, locale_set = locale_set) |> 
    step_lag(all_numeric_predictors(), 1:5) |> 
    step_naomit() |> 
    step_rm(date) |> 
    step_normalize(all_numeric_predictors())
}