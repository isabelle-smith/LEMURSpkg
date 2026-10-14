


#' Concatenate and Print without Spaces
#'
#' @param ... \R object(s), and any additional arguments to [cat()].
#' @param sep Strings to append after each object.
#'
#' @returns None (invisible NULL).
#' @export
#'
#' @seealso [cat()]
#'
#' @examples
#'
#' x <- c("test", "one", "two", "three")
#'
#' #cat(x) ## default `cat`
#' #cat("\n")
#'
#' #cat0(x) ## default `cat0`
#'
#'


cat0 <- function(..., sep="") {
  cat(..., sep=sep)
}

