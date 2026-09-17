

#' Make Cases for `mutate`
#'
#' @param case_v A character vector of expressions containing the code to create the new variables.
#' @param name_v A character vector of names for the new variables.
#'
#' @returns A list of parsed expressions (via [rlang::parse_exprs()]), ready to be spliced (see `!!!` aka [rlang::splice()]) and used in a [dplyr::mutate()] statement.
#'
#' @importFrom rlang parse_exprs
#' @importFrom purrr set_names
#'
#' @export
#'
#' @examples
#'
#' x_cases <- c("speed+dist","speed-dist","speed/dist")
#' x_names <- c("sum", "diff", "ratio")
#'
#' x_cars2 <- cars |> dplyr::mutate(!!!fn_mutate_cases(x_cases, x_names))
#'
#' format(x_cars2)
#'


fn_mutate_cases <- function(case_v, name_v){

  return( purrr::set_names(rlang::parse_exprs(case_v), name_v) )

}

