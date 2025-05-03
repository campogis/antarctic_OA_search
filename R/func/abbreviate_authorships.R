abbreviate_authorships <- function(oa_works_df) {

  oa_works_df <- oa_works_df |>
    mutate(citation = NA, .before = 2)
  
  for (i in seq(length.out = nrow(oa_works_df))) {
    x <- oa_works_df[i,]
    year <- x$publication_year
    a <- x$authorships[[1]]
    if (!inherits(a, "data.frame")) {
      auth <- "NA"
      cit <- paste0(auth, " (", year, ")")
    } else {
      auth <- sapply(strsplit(a$display_name, split = " "),
        FUN = tail, n = 1
      )
      if (length(auth) > 2) {
        auth <- paste0(auth[1], " et al.")
        cit <- paste0(paste0(auth, collapse = " "), " (", year, ")")
      } else {
        cit <- paste0(paste0(auth, collapse = " & "), " (", year, ")")
      }
      oa_works_df[i,"citation"] <- cit
    }
  }

  return(oa_works_df)
}
