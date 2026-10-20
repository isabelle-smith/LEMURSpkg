

#' Divider for Printing
#'
#' Returns a string divider in the format A(BA...). See **Details** for how the divider's length is calculated.
#'
#' @param length_out Numeric (value < 0). Desired minimum length of divider.
#' @param char_str *Optional.* String. Part A of alternating pattern. Defaults to `"-"`.
#' @param spac_str *Optional.* String. Part B of alternating pattern. Defaults to `" "`.
#'
#' @returns String divider, beginning (and ending with `char_str`). The divider length
#'  will always be greater than both `length_out` and the length of `char_str`.
#'
#' @details
#' The divider is built as `A + n(BA)`
#'  where `n = ceiling( (L - nchar(A)) / nchar(A+B) )`
#'  with `L` corresponding to `length_out`.
#'
#'
#' @export
#'
#' @examples
#'
#' ## Basic Examples:
#'
#' cat(divider(7), "\n") ## gives `- - - -`
#'
#' cat("Header:", "\n", divider(7), "\n", "results, description, etc.")
#'
#' cat(divider(7, char_str="*"), "\n")
#' cat(divider(7, char_str="*", spac_str="_"), "\n")
#'
#'
#' ## Variable Lengths:
#'
#' cat(divider(6, char_str="*", spac_str="_"), "\n")
#' cat(divider(7, char_str="*", spac_str="_"), "\n") ## same length as above
#'
#' cat(divider(5, char_str="**", spac_str="_"), "\n")
#' cat(divider(6, char_str="**", spac_str="_"), "\n") ## longer than above
#' cat(divider(7, char_str="**", spac_str="_"), "\n") ## same length as above
#' cat(divider(8, char_str="**", spac_str="_"), "\n") ## ...
#' cat(divider(9, char_str="**", spac_str="_"), "\n") ## even longer
#'


divider <- function(length_out, char_str="-", spac_str=" ") {


  ## stops - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if ( !all(is.numeric(length_out)) )         { stop("Input `length_out` value must be numeric.")}
  if ( any(length_out <= 0) )                 { stop("Input `length_out` value must be strictly positive.")}
  if ( any(typeof(char_str) != "character") ) { stop("Input `char_str` value must be characters only.") }
  if ( any(typeof(spac_str) != "character") ) { stop("Input `spac_str` value must be characters only.") }
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


  rep_str <- paste0(spac_str,
                    char_str)

  rep_tim <- ceiling( (length_out - nchar(char_str)) / nchar(rep_str) )

  divider <- ifelse(rep_tim>0,
                    paste0(char_str, strrep(rep_str, rep_tim)),
                    char_str)

  return(divider)

}

