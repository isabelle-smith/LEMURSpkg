
## Source: https://r-pkgs.org/data.html#sec-data-example-path-helper

#' Get path to LEMURSpkg example
#'
#' `LEMURSpkg` comes bundled with some example files in its `inst/extdata` directory. This function make them easy to access.
#'
#' @param path Name of file. If NULL, the example files will be listed.
#'
#' @export
#'


LEMURSpkg_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "LEMURSpkg"))
  } else {
    system.file("extdata", path, package = "LEMURSpkg", mustWork = TRUE)
  }
}
