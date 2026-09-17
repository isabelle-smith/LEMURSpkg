

#' Make Cases for `mutate`
#'
#' @param case_v A character vector of expressions containing the code to create the new variables.
#' @param name_v A character vector of names for the new variables.
#'
#' @returns A list of parsed expressions, ready to be spliced (see [rlang::splice()]) and used in [dplyr::mutate()].
#'
#' @importFrom rlang parse_exprs
#' @importFrom purrr set_names
#'
#' @export
#'
#' @seealso [rlang::parse_exprs()]
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

