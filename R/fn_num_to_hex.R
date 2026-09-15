

#' Convert Base-10 to Bijective Base-26
#' @description
#' Note that this function is **not vectorized**; see [lapply()], etc. to use with multiple inputs.
#'
#'
#' @param num A single number. Will be rounded and converted, if not already an integer.
#'
#' @returns A string that is the bijective base-26 representation of the input number,
#'    or `NA_character_` for non-positive input.
#' @export
#'
#' @details
#' Non-integers are rounded by `floor(num + 0.5)` (see *References*), then converted using [as.integer()].
#'
#'
#' @references
#' 1. Credit for the rounding method goes to Stack Overflow user "flodel" (1201032).
#'   The answer was posted Oct 2, 2012 at 10:55 and accessed Aug 11, 2026. Link (shows revisions):
#'   [https://stackoverflow.com/revisions/12688986/4](https://stackoverflow.com/revisions/12688986/4)
#'     - Code used: `floor(0.5 + x)`
#'
#' 2. More information on the meaning of bijective can be found at
#'   [https://en.wikipedia.org/wiki/Bijective_numeration](https://en.wikipedia.org/wiki/Bijective_numeration)
#'
#' @seealso [as.integer()] for expected conversion behavior.
#'
#' @examples
#' ## basic use:
#' fn_num_to_hex(1)    ## "A"
#' fn_num_to_hex(26)   ## "Z"
#' fn_num_to_hex(27)   ## "AA"
#'
#' ## non-positives:
#' fn_num_to_hex(0)    ## NA
#' fn_num_to_hex(-1)   ## NA
#'
#' ## rounding:
#' fn_num_to_hex(8.75)  ## "I"
#' floor(8.75 + 0.5)    ## 9
#' fn_num_to_hex(9)     ## "I"
#'


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

