list.files.github <- function(path) {
  req <- GET(path)
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  return(filelist)
}

read_csv_and_add_path_as_column <- function(filepath, delim = ",") {
  filepath |>
    fread(sep = delim) |>
    mutate(identifier = filepath) |>
    as_tibble()
}

import_data_allwells_github <- function(path) {
  csv_files <- PATH_GITHUB_GROUNDWATERCHALLENGE |>
    list.files.github()

  data_gwl <- csv_files |>
    keep(str_detect, "heads.csv") %>%
    str_c(PATH_GITHUB_REPO, "main/", .) |>
    str_replace("github", "raw.github") |>
    map(read_csv_and_add_path_as_column) |>
    map_dfr(~ rename(.x, any_of(c("Date" = "V1")))) |>
    clean_names() |>
    rename(gwl = head) |>
    mutate(identifier = word(identifier, start = -2, sep = "/")) |>
    drop_na(date, identifier)

  data_predictors <- csv_files |>
    keep(str_detect, "input_data.csv") %>%
    str_c(PATH_GITHUB_REPO, "main/", .) |>
    str_replace("github", "raw.github") |>
    map(read_csv_and_add_path_as_column) |>
    map_if(
      .p = ~ .x |> names() |> str_detect("TMAX") |> any(),
      .f = ~ .x |> group_by(V1) |> mutate(tg = mean(c(TMAX, TMIN)))
           ) |>
    map_dfr(~ rename(.x, any_of(c(
      "time" = "V1",
      "rr" = "PRCP",
      "tx" = "TMAX",
      "tn" = "TMIN",
      "et" = "ET",
      "river" = "Stage_m"
    )))) |>
    clean_names() |>
    rename(date = time) |>
    mutate(identifier = word(identifier, start = -2, sep = "/")) |>
    drop_na(date, identifier)

  data_gwl |>
    full_join(data_predictors, by = c("identifier", "date")) |>
    rename(well_id = identifier) |>
    relocate(well_id, date, gwl) |>
    arrange(well_id, date) |>
    mutate(date = date |> as.character() |> lubridate::as_date())
}