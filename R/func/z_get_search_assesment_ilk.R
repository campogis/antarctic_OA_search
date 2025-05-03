get_search_assessment_ilk <- function(st_ilk_fn, search_terms){
  result <- list(
    excl = assess_search_term(
      st = readLines(st_ilk_fn),
      AND_term = search_terms$monitoring,
      remove = " OR$",
      excl_others = TRUE, 
      verbose = TRUE
    ),
    incl = assess_search_term(
      st = readLines(st_ilk_fn),
      AND_term = search_terms$monitoring,
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
  attributes(result)$AND_term <- search_terms$monitoring

  return(result)
}