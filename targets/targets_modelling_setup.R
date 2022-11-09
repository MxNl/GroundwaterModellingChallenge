targets_modelling_setup <- list(
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

  # Recipes -----------------------------------------------------------------

  tar_target(
    recipe_pure,
    initial_split |>
      training() |>
      make_recipe_pure(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_pure_but_all_standard_predictors,
    initial_split |>
      training() |>
      make_recipe_pure_but_all_standard_predictors(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_lag_pca_zv_dateext,
    initial_split |>
      training() |>
      make_recipe_recipe_lag_pca_zv_dateext(),
    pattern = map(initial_split),
    iteration = "list"
  ),

  # Model Definitions -------------------------------------------------------

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
  #### NNetar
  tar_target(
    tune_grid_nnetar,
    make_tune_grid_nnetar()
  ),
  tar_target(
    model_grid_nnetar,
    make_model_grid_nnetar(tune_grid_nnetar)
  ),
  #### automl
  tar_target(
    model_automl,
    make_model_automl(),
  ),

  # Workflow ----------------------------------------------------------------

  tar_target(
    workflow_set,
    make_workflow_set(
      recipes = list(
        recipe_lag_pca_zv_dateext,
        recipe_pure_but_all_standard_predictors
      ),
      models = c(
        # model_automl
        # model_grid_xgboost$.models
        model_grid_prophet$.models,
        model_grid_nnetar$.models
      )
    ),
    pattern = map(
      recipe_lag_pca_zv_dateext,
      recipe_pure_but_all_standard_predictors
    ),
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
