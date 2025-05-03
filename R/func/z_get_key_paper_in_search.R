get_key_paper_in_search <- function(key_paper, key_works, search_terms) {

  dois <- key_paper$doi |>
    IPBES.R::doi_clean()
  dois <- dois[!is.na(dois)]

  kp_in_search <- openalexR::oa_fetch(
    entity = "works",
    title_and_abstract.search = search_terms$full,
    doi = dois,
    options = list(
      select = c(
        "doi"
      )
    )
  ) |>
    unlist() |>
    unname()


  result <- key_works |>
    dplyr::mutate(
      in_search = doi %in% kp_in_search
    ) |>
    dplyr::select(
      id,
      in_search,
      doi,
      citation,
      title,
      abstract
    )
  
    attributes(result)$timestamp <- Sys.time()
  
  return(result)
}