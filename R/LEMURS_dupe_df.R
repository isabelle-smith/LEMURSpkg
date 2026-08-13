
#' LEMURS Dupe Data Frame
#'
#' A toy data set meant to represent duplicate survey response rows.
#'
#' @format ## `LEMURS_dupe_df`
#' A data frame with 29 rows and 8 columns:
#' \describe{
#'   \item{surveyID}{integer; value of 3 for all}
#'   \item{recordID}{string; one of CA, KS, LA, MD, MI, ND, NH, NM, OR, VT}
#'   \item{rec_date}{POSIXct; one of "1999-12-31" "2000-01-01" "2000-01-02" "2000-02-03"}
#'   \item{finished}{integer; 1 if progress is 100, 0 otherwise}
#'   \item{progress}{integer; between 0 and 100}
#'   \item{variablR}{string; unique to each row but nonsensical}
#'   \item{variablX}{integer; one of 1, 22, NA}
#'   \item{variablY}{integer; one of 1, 22, NA}
#'   \item{variablC}{string; one of "a", "txt", NA}
#'   \item{orig_row}{string; unique to each row but nonsensical}
#' }
#' @source created by Izzy, 2026
"LEMURS_dupe_df"
