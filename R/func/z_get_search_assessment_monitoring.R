get_search_assessment_monitoring <- function(st_monitoring_fn, search_terms){
  result <- list(
    excl = assess_search_term(
      st = readLines(st_monitoring_fn),
      AND_term = search_terms$ilk,
      remove = " OR$",
      excl_others = TRUE, 
      verbose = TRUE
    ),
    incl = assess_search_term(
      st = readLines(st_monitoring_fn),
      AND_term = search_terms$ilkg,
      remove = " OR$",
      excl_others = FALSE, 
      verbose = TRUE
    )
  ) |>
    do.call(what = cbind) |>
    dplyr::rename(
      term = excl.term
    ) |>
    dplyr::mutate(
      incl.term = NULL
    )

  attributes(result)$timestamp <- Sys.time()
  attributes(result)$AND_term <- search_terms$ilk

  return(result)
}