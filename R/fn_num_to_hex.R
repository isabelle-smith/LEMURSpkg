

#' Convert Base-10 to Bijective Base-26
#'
#' @param num Any positive number. Will be rounded and converted, if not already an integer.
#'
#' @returns A string that is the bijective (no 0) base-26 representation of the input number.
#' @export
#'
#' @details
#' Non-integers are rounded by `floor(num + 0.5)` *(see References)*, then converted using [as.integer()].
#' Non-positive numbers return `NA_character_`.
#'
#'
#' @references
#' Credit for the rounding method goes to Stack Overflow user "flodel" (1201032).
#'   The answer was posted Oct 2, 2012 at 10:55 and accessed Aug 11, 2026.
#'   Excerpt: `floor(0.5 + x)`
#'   Direct link: https://stackoverflow.com/a/12688986
#'   Most recent: https://stackoverflow.com/revisions/12688986/4
#'
#'
#' @seealso [as.integer()] for expected conversion behavior.
#'
#' @examples
#'
#' fn_num_to_hex(1)    ## "A"
#' fn_num_to_hex(26)   ## "Z"
#' fn_num_to_hex(27)   ## "AA"
#'
#' fn_num_to_hex(0)    ## NA
#' fn_num_to_hex(-1)   ## NA
#'
#' fn_num_to_hex(8.75)  ## "I"
#' floor(8.75 + 0.5)    ## 9
#' fn_num_to_hex(9)     ## "I"


fn_num_to_hex <- function(num) {

  ## checking . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  if (typeof(num) != "integer") { num <- as.integer(floor(num + 0.5)) }

  if (num <= 0) { return(NA_character_) }

  ## function . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  hex <- ""

  while (num > 0) {
    num <- num - 1
    r <- num %% 26
    hex <- paste0(LETTERS[r + 1], hex)
    num <- num %/% 26
  }

  return(hex)

}

