assemble_search_terms <- function(st_monitoring_fn, st_ilk_fn
){
  search_terms <- list(
    monitoring = paste0(
      readLines(st_monitoring_fn),
      collapse = "\n"
    ),
    ilk = paste0(
      readLines(st_ilk_fn),
      collapse = "\n"
    )
  )
  
  search_terms$full <- paste0(
    "(\n", search_terms$monitoring, "\n) AND (\n", search_terms$ilk, "\n)"
  )

  return(search_terms)
}