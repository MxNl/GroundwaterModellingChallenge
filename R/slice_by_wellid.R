slice_by_wellid <- function(x, nth = 1) {
  x |> 
    group_by(well_id) %>%
    group_split() %>%
    magrittr::extract(nth) |> 
    reduce(bind_rows)
}
