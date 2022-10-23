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
    recipe_basic,
    initial_split |>
      training() |>
      make_recipe_basic(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe,
    initial_split |>
      training() |>
      make_recipe(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_1,
    initial_split |>
      training() |>
      make_recipe_1(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  #### svm
  tar_target(
    tune_grid_svm,
    make_tune_grid_svm()
  ),
  tar_target(
    model_grid_svm,
    make_model_grid_svm(tune_grid_svm)
  ),
  #### xgboost
  tar_target(
    tune_grid_xgboost,
    make_tune_grid_xgboost()
  ),
  tar_target(
    model_grid_xgboost,
    make_model_grid_xgboost(tune_grid_xgboost)
  ),
  #### prophet
  tar_target(
    tune_grid_prophet,
    make_tune_grid_prophet()
  ),
  tar_target(
    model_grid_prophet,
    make_model_grid_prophet(tune_grid_prophet)
  ),
  #### automl
  tar_target(
    model_automl,
    make_model_automl()
  ),
  ###########
  tar_target(
    workflow_set,
    make_workflow_set(
      recipes = list(recipe_basic, recipe, recipe_1),
      models = c(model_grid_prophet$.models)
    ),
    pattern = map(recipe_basic, recipe, recipe_1),
    iteration = "list"
  ),
  tar_target(
    fitted_models,
    fit_models(
      training(initial_split),
      workflow_set
    ),
    pattern = map(initial_split, workflow_set),
    iteration = "list"
  ),
  tar_target(
    predicted_test_split,
    modeltime_calibrate(
      fitted_models,
      testing(initial_split)
    ),
    pattern = map(fitted_models, initial_split),
    iteration = "list"
  ),
  tar_target(
    model_accuracy,
    modeltime_accuracy(predicted_test_split) |>
      table_modeltime_accuracy(.interactive = FALSE),
    pattern = map(predicted_test_split),
    iteration = "list"
  ),
  tar_target(
    plot_model_accuracy,
    fitted_models |>
      modeltime_forecast(
        new_data    = testing(initial_split),
        actual_data = data_modelling,
        keep_data   = TRUE
      ) %>%
      plot_modeltime_forecast(
        .facet_ncol  = 3,
        .interactive = TRUE
      ),
    pattern = map(fitted_models, initial_split, data_modelling),
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
