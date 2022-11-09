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
      rank_results(rank_metric = "rsq", select_best = FALSE),
    pattern = map(fitted_models_cl),
    iteration = "list"
  ),
  tar_target(
    best_workflow_cl,
    fitted_models_cl |> 
      fit_best_model(best_performance_cl, initial_split),
    pattern = map(fitted_models_cl, best_performance_cl, initial_split),
    iteration = "list"
  )
)