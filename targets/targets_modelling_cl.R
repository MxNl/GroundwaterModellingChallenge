targets_modelling_cl <- list(
  tar_target(
    fitted_models_cl,
    tune_resampling(
      workflow_set,
      resampling
    ),
    pattern = map(workflow_set, resampling),
    iteration = "list"
  ),
  tar_target(
    best_performance_cl,
    fitted_models_cl |>
      rank_results(rank_metric = "rmse", select_best = FALSE),
    pattern = map(fitted_models_cl),
    iteration = "list"
  ),
  tar_target(
    best_models_selection,
    best_performance_cl |>
      select_best_models(fitted_models_cl),
    pattern = map(best_performance_cl, fitted_models_cl),
    iteration = "list"
  ),
  tar_target(
    model_ensemble,
    stacks() |> stacks::add_candidates(best_models_selection),
    pattern = map(best_models_selection),
    iteration = "list"
  ),
  tar_target(
    blended_ensemble,
    model_ensemble |> 
      blend_predictions(mixture = seq(0,1,by=0.1)),
    pattern = map(model_ensemble),
    iteration = "list"
  ),

# Repetitions -------------------------------------------------------------

  tar_target(
    fit_repeats,
    1:REPEATS
  ),
  tar_target(
    fitted_ensemble,
    blended_ensemble |> 
      fit_members(),
    pattern = cross(blended_ensemble, fit_repeats),
    iteration = "list"
  ),
  tar_target(
    initial_split_repeats,
    initial_split,
    pattern = cross(initial_split, fit_repeats),
    iteration = "list"
  ),
  tar_target(
    data_training_repeats,
    initial_split %>% training(),
    pattern = cross(initial_split, fit_repeats),
    iteration = "list"
  ),
  tar_target(
    data_modelling_repeats,
    data_modelling,
    pattern = cross(data_modelling, fit_repeats),
    iteration = "list"
  ),
  tar_target(
    data_prediction_repeats,
    data_prediction,
    pattern = cross(data_prediction, fit_repeats),
    iteration = "list"
  ),
  tar_target(
    data_train_test_pred_full,
    data_modelling |> 
      bind_rows(data_prediction),
    pattern = map(data_modelling, data_prediction),
    iteration = "list"
  ),
  tar_target(
    data_train_test_pred_full_repeats,
    data_train_test_pred_full,
    pattern = cross(data_train_test_pred_full, fit_repeats),
    iteration = "list"
  ),

# Predictions -------------------------------------------------------------

  tar_target(
    predictions_ensemble_test,
    testing(initial_split_repeats) %>%
      bind_cols(predict(fitted_ensemble, .)),
    pattern = map(initial_split_repeats, fitted_ensemble),
    iteration = "list"
  ),
  tar_target(
    predictions_ensemble_training,
    data_training_repeats %>%
      bind_cols(predict(fitted_ensemble, .)),
    pattern = map(data_training_repeats, fitted_ensemble),
    iteration = "list"
  ),
  tar_target(
    predictions_ensemble_training_full,
    data_modelling_repeats %>%
      bind_cols(predict(fitted_ensemble, .)),
    pattern = map(data_modelling_repeats, fitted_ensemble),
    iteration = "list"
  ),
  tar_target(
    predictions_ensemble_test_full,
    data_prediction_repeats %>%
      bind_cols(predict(fitted_ensemble, .)),
    pattern = map(data_prediction_repeats, fitted_ensemble),
    iteration = "list"
  ),
  tar_target(
    predictions_train_test_pred_full,
    data_train_test_pred_full_repeats %>%
      bind_cols(predict(fitted_ensemble, .)),
    pattern = map(data_train_test_pred_full_repeats, fitted_ensemble),
    iteration = "list"
  ),

# Plotting ----------------------------------------------------------------

  tar_target(
    plot_results_obs_and_preds,
    make_plot_results_obs_and_preds(predictions_ensemble_test)
  ),
  tar_target(
    plot_results_obs_and_preds_wtrain,
    make_plot_results_obs_and_preds_wtrain(
      predictions_ensemble_training,
      predictions_ensemble_test
    )
  ),
  tar_target(
    plot_results_obs_and_preds_wtrain_full,
    make_plot_results_obs_and_preds_wtrain_full(
      predictions_ensemble_training,
      predictions_ensemble_test,
      predictions_ensemble_test_full
    )
  ),
  tar_target(
    plot_results_obs_and_preds_train_test_pred_full,
    make_plot_results_obs_and_preds_train_test_pred_full(
      predictions_ensemble_test,
      predictions_train_test_pred_full
    )
  ),
  tar_target(
    performance_table,
    make_performance_table(predictions_ensemble_test)
  ),
  tar_target(
    performance_table_training,
    make_performance_table(predictions_ensemble_training),
    priority = 1
  )
)