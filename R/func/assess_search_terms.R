#' Assess Search Term
#'
#' This function assesses the search term by counting the number of occurrences in a given text corpus.
#'
#' @param st The search term to be assessed. Each line should be one sub-term. Usually, these are combined by `OR`.
#' @param remove A regular expression pattern to remove from the search term.
#' @param excl_others Logical indicating whether to exclude other search terms from the count.
#'
#' @return A data frame with the search term and the corresponding count.
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom openalexR oa_fetch
#'
#' @md
#'
#' @examples
#' assess_search_term(list("climate OR", "change"))
#'
#' @keywords internal
assess_search_term <- function(
    st = NULL,
    AND_term = NULL,
    remove = " OR$",
    excl_others = FALSE,
    verbose = FALSE
) {
  st <- gsub(pattern = remove, replacement = "", st)
  result <- data.frame(
    term = st, 
    count = pbapply::pblapply(
      st,
      function(x) {
        if (excl_others) {
          excl <- st[!(st %in% x)]
          searchterm <- paste0("(", x, ") NOT (", paste0(excl, collapse = " OR "), ")")
        } else {
          searchterm <- x
        }

        if (is.null(AND_term)) {
          searchterm <- compact_st(searchterm) 
        } else {
          AND_term <- gsub(pattern = remove, replacement = "", AND_term) #YS addition
          searchterm <- compact_st(paste0("(", AND_term, ") AND (", searchterm, ")"))
          print(searchterm)
        }

        openalexR::oa_fetch(
          title_and_abstract.search = searchterm,
          output = "list",
          count_only = TRUE,
          verbose = verbose
        )$count
        #print()
      }
    ) |>
      unlist()
  )
  ## Add attributes
  attributes(result)$timestamp <- Sys.time()
  attributes(result)$AND_term <- AND_term
  attributes(result)$excl_others <- excl_others
  return(result)
}
