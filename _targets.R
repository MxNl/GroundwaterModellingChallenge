library(targets)
library(tarchetypes)
library(renv)

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c(
    "janitor",
    "here",
    "yaml",
    "lubridate",
    "httr",
    "data.table",
    "parallel",
    "doParallel",
    "relayer",
    "rlang",
    "modeltime",
    "ranger",
    "timetk",
    "workflowsets",
    "tidymodels",
    "stacks",
    "tidyverse"
    ),
  memory = "transient",
  garbage_collection = TRUE
)

# Source functions
path_functions <- "R"
here::here(path_functions) |>
  list.files() |>
  {\(x) here::here(path_functions, x)}() |>
  purrr::map(source)

# Source target pipelines
path_targets <- "targets"
here::here(path_targets) |>
  list.files() |>
  {\(x) here::here(path_targets, x)}() |>
  purrr::map(source)


if(PARALLEL) plan(multisession)

tar_seed()

# Define targets
c(
  targets_import,
  targets_preprocessing,
  targets_modelling_setup,
  targets_modelling_cl,
  targets_export
)