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
  #### rf
  tar_target(
    tune_grid_rf,
    make_tune_grid_rf()
  ),
  tar_target(
    model_grid_rf,
    make_model_grid_rf(tune_grid_rf)
  ),

  # Workflow ----------------------------------------------------------------

  tar_target(
    workflow_set,
    make_workflow_set(
      recipes = list(
        # recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca,
        # recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_nodate,
        recipe_wolag_logtrans_linimp_norm_zv_corr,
        recipe_lag_logtrans_linimp_norm_zv_corr
        # recipe_lag_logtrans_linimp_norm_zv_corr_pca
        ),
      models = c(
        model_grid_rf$.models,
        model_grid_mlp$.models,
        model_grid_svm$.models,
        model_grid_svmpoly$.models
      )
    ),
    pattern = map(
      # recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca,
      # recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_nodate,
      recipe_wolag_logtrans_linimp_norm_zv_corr,
      recipe_lag_logtrans_linimp_norm_zv_corr
      # recipe_lag_logtrans_linimp_norm_zv_corr_pca
      ),
    iteration = "list"
  )
)