get_search_term_hits <- function(search_terms){
  st <- names(search_terms)
  search_term_hits <- lapply(
    st,
    function(stn) {
      message("getting '", stn, "' ...")
      search <- search_terms[[stn]] |>
        compact_st()
      openalexR::oa_fetch(title_and_abstract.search = search, count_only = TRUE, verbose = TRUE) |>
        unlist()
    }
  ) |>
    do.call(what = rbind) |>
    as.data.frame() |>
    dplyr::mutate(page = NULL, per_page = NULL) |>
    dplyr::mutate(count = formatC(count, format = "f", big.mark = ",", digits = 0))

  rownames(search_term_hits) <- st |>
    gsub(pattern = "st_", replacement = "") |>
    gsub(pattern = "f_", replacement = "")

  attributes(search_term_hits)$timestamp <- Sys.time()

  return(search_term_hits)
}