library(targets)
# library(modeltime.gluonts)
library(tarchetypes)
library(renv)

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c(
    "janitor",
    "lubridate",
    "httr",
    "data.table",
    "doParallel",
    "rlang",
    "modeltime.gluonts",
    "modeltime",
    "tidymodels",
    "tidyverse"
    ),
  memory = "transient",
  garbage_collection = TRUE
)

# library(showtext)
# Add font for ggplots
# font_add("Corbel", regular = "C:\\Windows\\Fonts\\corbel.ttf")

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

# Define targets
c(
  targets_import,
  targets_preprocessing,
  targets_modelling_setup,
  targets_modelling_cl
  # targets_modelling_mt
)