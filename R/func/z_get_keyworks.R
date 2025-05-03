get_key_works <- function(key_paper){
  dois <- key_paper$doi |>
    IPBES.R::doi_clean()
  dois <- dois[!is.na(dois)]

  kw <- openalexR::oa_fetch(
    entity = "works",
    doi = dois
  ) |>
    abbreviate_authorships()

  attributes(kw)$timestamp <- Sys.time()

  return(kw)
}