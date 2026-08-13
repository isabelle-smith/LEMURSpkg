

#' Test Equality, Accounting for NAs
#' @description
#' `fn_equal_with_na` returns `TRUE` if both inputs are `NA`,
#' the value of `one_na_eq` if only one input is `NA`, and
#' the value of `x1==x2` otherwise.
#'
#' If inputs are of differing lengths, both of these things must be true to prevent errors:
#' 1. `mixed_length` is set to `TRUE` (not the default)
#' 2. the length of the longer input is a multiple of the shorter input
#'
#'
#' @param x1 Any atomic value or vector. Value to be compared to `x2`.
#' @param x2 Any atomic value or vector. Value to be compared to `x1`.
#' @param one_na_eq Any atomic value. Will be returned if only one of `x1` or `x2` is `NA`. Defaults to `FALSE`.
#' @param mixed_lengths Logical. Whether or not to accept input of differing lengths.
#'
#' @returns An object of length `max(length(x1), length(x2))` with the same type as `one_na_eq` (default is `logical`).
#' @export
#'
#' @seealso [atomic()] (redirects to `vector`)
#'
#' @examples
#'
#' ## single values:
#' fn_equal_with_na(1, 1)    ## TRUE
#' fn_equal_with_na(1, 2)    ## FALSE
#' fn_equal_with_na(NA, NA)  ## TRUE
#' fn_equal_with_na(1, NA)   ## FALSE (default)
#'
#' ## changing `one_na_eq`
#' fn_equal_with_na(1, NA, one_na_eq=TRUE)     ## TRUE
#' fn_equal_with_na(1, NA, one_na_eq=999)      ## 999
#' fn_equal_with_na(1, NA, one_na_eq="error")  ## "error"
#' fn_equal_with_na(1, NA, one_na_eq=NA)  ## "error"
#'
#' ## multiple values:
#' vec1 <- c(1, 1, NA, 1, 1, NA, 1, 1, 1, NA)
#' vec2 <- c(1, 2, NA, 4, 5, 6, 7, NA, 9, NA)
#' eq_F <- fn_equal_with_na(vec1, vec2)                   ## implied `one_na_eq=FALSE`
#' eq_T <- fn_equal_with_na(vec1, vec2, one_na_eq=TRUE)
#' data.frame(vec1, vec2, eq_F, eq_T)
#'
#' ## mixing types:
#' fn_equal_with_na(1, 1.0)  ## TRUE
#' fn_equal_with_na(1, "a")  ## FALSE
#'
#' ## mixed length input:
#' # fn_equal_with_na(1, 1:5)             ## not run: <<error>> (implied `mixed_length=FALSE`)
#' fn_equal_with_na(1, 1:5,
#'                  mixed_length=TRUE)  ## TRUE FALSE FALSE FALSE FALSE
#'


fn_equal_with_na <- function(x1,
                             x2,
                             one_na_eq=FALSE,
                             mixed_lengths=FALSE) {

  length1 <- length(x1)
  length2 <- length(x2)

  ## stop actions: ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  if ( (mixed_lengths!=TRUE & length1!=length2) ) {
    err_mix <- paste0("Inputs are not the same length:\n",
                      "[x1] - ", length1, "   &   [x2] - ", length2)
    stop(err_mix)
  }

  if ( max(length1, length2) %% min(length1, length2) != 0) {
    err_mod <- paste0("Inputs are not of compatible lengths:\n",
                     "[x1] - ", length1, "   &   [x2] - ", length2)
    stop(err_mod)
  }

  ## ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  ifelse((is.na(x1) & is.na(x2)),
         yes = TRUE,
         no  = ifelse((is.na(x1) | is.na(x2)),
                   yes = one_na_eq,
                   no  = x1==x2))

}

