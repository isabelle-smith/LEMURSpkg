

#' Make Cases for `mutate`
#'
#' @param case_v A character vector of names for the new variables.
#' @param name_v A character vector of expressions...???
#'
#' @returns A spliced list of expressions, ready to use in [dplyr::mutate()].
#' @export
#'
#' @seealso [rlang::parse_exprs()] and [rlang::splice()]
#'
#' @examples
#' ## tbd
#'


fn_mutate_cases <- function(case_v, name_v){

  !!!( purrr::set_names(rlang::parse_exprs(case_v), name_v) )

}

