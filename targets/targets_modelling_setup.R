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
    resampling_cv,
    initial_split |>
      training() |>
      make_resampling_cv(),
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
    recipe_wolag_logtrans_linimp_norm,
    initial_split |>
      training() |>
      make_recipe_wolag_logtrans_linimp_norm(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca,
    initial_split |>
      training() |>
      make_recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_wolag_logtrans_linimp_norm_zv_corr,
    initial_split |>
      training() |>
      make_recipe_wolag_logtrans_linimp_norm_zv_corr(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_lag_logtrans_linimp_norm_zv_corr,
    initial_split |>
      training() |>
      make_recipe_lag_logtrans_linimp_norm_zv_corr(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_lag_logtrans_linimp_norm_zv_corr_pca,
    initial_split |>
      training() |>
      make_recipe_lag_logtrans_linimp_norm_zv_corr_pca(),
    pattern = map(initial_split),
    iteration = "list"
  ),
  tar_target(
    recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_nodate,
    initial_split |>
      training() |>
      make_recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_nodate(),
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
  #### svmpoly
  tar_target(
    tune_grid_svmpoly,
    make_tune_grid_svmpoly()
  ),
  tar_target(
    model_grid_svmpoly,
    make_model_grid_svmpoly(tune_grid_svmpoly)
  ),
  #### mlp
  tar_target(
    tune_grid_mlp,
    make_tune_grid_mlp()
  ),
  tar_target(
    model_grid_mlp,
    make_model_grid_mlp(tune_grid_mlp)
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
  #### gluonts
  tar_target(
    tune_grid_gluontslstm,
    make_tune_grid_gluontslstm()
  ),
  tar_target(
    model_grid_gluontslstm,
    make_model_grid_gluontslstm(tune_grid_gluontslstm)
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
        recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca,
        # recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_nodate,
        recipe_wolag_logtrans_linimp_norm_zv_corr,
        recipe_lag_logtrans_linimp_norm_zv_corr,
        recipe_lag_logtrans_linimp_norm_zv_corr_pca),
      models = c(
        model_grid_xgboost$.models,
        model_grid_mlp$.models,
        model_grid_svm$.models,
        model_grid_svmpoly$.models
        # model_grid_prophet$.models,
        # model_grid_nnetar$.models
      )
    ),
      # filter(!str_detect(wflow_id, "recipe_1_mlp|recipe_3_mlp|recipe_4_mlp|recipe_5_mlp")) |> 
      # filter(str_detect(wflow_id, "recipe_1|recipe_3|recipe_4|recipe_5|recipe_2_mlp")),
    pattern = map(
      recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca,
      # recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_nodate,
      recipe_wolag_logtrans_linimp_norm_zv_corr,
      recipe_lag_logtrans_linimp_norm_zv_corr,
      recipe_lag_logtrans_linimp_norm_zv_corr_pca),
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
