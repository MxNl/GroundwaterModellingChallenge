targets_modelling <- list(
  tar_target(
    initial_split,
    data_modelling |> 
      initial_time_split(prop = INITIAL_PROP),
    pattern = map(data_modelling),
    iteration = "list"
  ),
  tar_target(
    resampling,
    initial_split |> 
      training() |> 
      make_resampling(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    plot_resamplingstrategy,
    resampling |>
      timetk::tk_time_series_cv_plan() |> 
      timetk::plot_time_series_cv_plan(date, gwl, .interactive = FALSE),
    pattern = map(resampling),
    iteration = "list"
  ),
  tar_target(
    recipe,
    initial_split |> 
      training() |> 
    make_recipe(),
    pattern = map(initial_split),
    iteration = "list"
  )
)


# targets_modelling <- list(
#   tar_target(
#     control_grid_stack,
#     make_stack_control_grid()
#   ),
#
#   tar_target(
#     train_test_split,
#     make_train_test_split(
#       data_features_target
#     ),
#     pattern = map(data_features_target),
#     iteration = "list"
#   ),
#
#   tar_target(
#     resampling_strategy_cv,
#     make_resampling_strategy(
#       train_test_split
#     ),
#     pattern = map(train_test_split),
#     iteration = "list"
#   ),
#
#   tar_target(
#     preprocessing_recipe,
#     make_recipe(
#       train_test_split
#     ),
#     pattern = map(train_test_split),
#     iteration = "list"
#   )
# )