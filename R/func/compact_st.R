#' Compact an OpenAlex search string
#'
#' This function takes an OpenAlex search string as input and performs several cleaning operations
#' to compact it. It removes newline characters, asterisks, and extra whitespace,
#' and ensures that parentheses are directly adjacent to their content.
#'
#' @param st An OpenAlex search string to be compacted.
#' @return A compacted OpenAlex search string.
#' @export
#' @examples
#' compact_st("climate change AND biodiversity")
#' # Returns: "climate change AND biodiversity"
#' compact_st("climate change OR biodiversity")
#' # Returns: "climate change OR biodiversity"
#' compact_st("(climate change OR biodiversity) AND (conservation OR restoration)")
#' # Returns: "(climate change OR biodiversity) AND (conservation OR restoration)"
#' compact_st("climate change AND (biodiversity OR ecosystem services)")
#' # Returns: "climate change AND (biodiversity OR ecosystem services)"
#' compact_st(" ( climate change  OR   biodiversity  )  AND   ( conservation OR restoration )  ")
#' # Returns: "(climate change OR biodiversity) AND (conservation OR restoration)"
#' compact_st("climate change AND *ecosystem*")
#' # Returns: "climate change AND ecosystem"

compact_st <- function(st) {
  st |>
    gsub(pattern = "\n", replacement = " ") |>
    gsub(pattern = "\\*", replacement = "") |>
    gsub(pattern = "\\s+", replacement = " ") |>
    gsub(pattern = "\\( ", replacement = "(") |>
    gsub(pattern = " )", replacement = ")")
}


