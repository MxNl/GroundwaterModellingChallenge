targets_modelling_mt <- list(
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
    modeltime_accuracy(
      predicted_test_split,
      metric_set = default_forecast_accuracy_metric_set(nse())
    ) |>
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
  ),
  tar_target(
    best_model_refit,
    model_accuracy |>
      arrange(-rsq, rmse) |> 
      slice(1) |> 
      modeltime_refit(),
    pattern = map(model_accuracy),
    iteration = "list"
  ),
  tar_target(
    predictions,
    best_model_refit |>
      modeltime_forecast(
        data_prediction
      ),
    pattern = map(best_model_refit, data_prediction),
    iteration = "list"
  )
)