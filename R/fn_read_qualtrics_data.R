

#' @title Read-in and lightly clean a Qualtrics CSV.
#'
#' @description
#' Description goes here.
#'
#' @param full_file_path File path to CSV file from Qualtrics. Assumes headers and 2 rows of Qualtrics info are present.
#' @param col_types_list Optional. List of column types for [readr::read_csv()].
#' @param unique_id One of "PID", "record_id", or "uvmid+uvmSurveyID". Column(s) specified must be present in file.
#' @param drop_cols Optional. Character vector of columns to remove. Not required to be present in file.
#' @param num_vars Optional. Character vector of columns to convert to numeric via [as.numeric()]. Not required to be present in file.
#' @param int_vars Optional. Character vector of columns to convert to integer via [as.integer()]. Not required to be present in file.
#' @param key_df Data frame with columns uvmid and record_id; used only if `unique_id="uvmid+uvmSurveyID"`.
#'
#' @details
#' Columns assumed present in file: `unique_id`, StartDate, EndDate, Duration (in seconds). Ideally, columns Finished and Progress also exist.
#'
#' Qualtrics columns that are removed: Status, IPAddress, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage.
#'
#' Qualtrics columns that are kept: StartDate, EndDate, Progress, Duration (in seconds), Finished, RecordedDate, ResponseId, ResponseID, SurveyID, and any user-created others.
#'
#' @returns Data frame without `drop_cols`, other Qualtrics columns, and rows that have NA in `unique_id`.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ## ADD EXAMPLES
#' ## put file(s) in `extdata` directory


fn_read_qualtrics_data <- function(full_file_path,
                                   col_types_list=list(.default = "c"),
                                   unique_id,
                                   drop_cols=c(),
                                   num_vars=c(),
                                   int_vars=c(),
                                   key_df=NULL) {



  ## helper fx(s) _ _ _ _ _ _ _ _ _ _ _ _ _ _

  ## post: https://www.reddit.com/r/rstats/s/NSb2eg6Cj5
  ## comment: https://www.reddit.com/r/rstats/comments/16vbzaf/comment/k2r6a4q/
  ## monad: https://youtu.be/bK-Tz-GLfOs?si=gSY2lh9CJQ2VMDTI

  do_if <- function(df, cond, f){
    if(cond) f(df) else df
  }

  ## _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _



  ## column names only...
  df_names <- readr::read_csv(full_file_path,
                              col_names = FALSE,
                              n_max = 1,
                              progress = FALSE,
                              show_col_types = FALSE) |> as.vector(mode="character")



  ## full dataframe...
  df <- readr::read_csv(full_file_path,
                        col_names = df_names,
                        skip = 3,
                        col_types = col_types_list,
                        progress = FALSE,
                        show_col_types = FALSE) |>


    ## drop rows 1+2 (Qualtrics info) {all}:
    dplyr::slice(-c(1,2)) |>


    ## drop columns (Qualtrics info) {all}
    dplyr::select(-dplyr::any_of( c("Status", "IPAddress",               ## kept: StartDate, EndDate,
                                    "RecipientLastName", "RecipientFirstName",          ##       Progress, Duration (in seconds), Finished,
                                    "RecipientEmail", "ExternalReference",              ##       RecordedDate, ResponseId, ResponseID, SurveyID
                                    "LocationLatitude", "LocationLongitude",            ##
                                    "DistributionChannel", "UserLanguage") )) |>        ## also: uvmid, uvmSurveyID / PID / SC0, Score / etc.


    ## rename to remove spaces/parentheses {all}
    dplyr::rename(dplyr::any_of( c(Duration = "Duration (in seconds)") )) |>


    ## drop any other columns:
    do_if(length(drop_cols) > 0,
          function(df) dplyr::select(df, -dplyr::any_of(drop_cols)) ) |>


    ## change listed columns types:
    do_if(length(num_vars) > 0,
          function(df) dplyr::mutate(df, dplyr::across(.cols=dplyr::any_of(num_vars), .fns=as.numeric)) ) |>
    do_if(length(int_vars) > 0,
          function(df) dplyr::mutate(df, dplyr::across(.cols=dplyr::any_of(int_vars), .fns=as.integer)) ) |>


    ## change date columns from strings to POSIXct {all}:
    dplyr::mutate(DateSt = as.POSIXct(.data$StartDate, format="%Y-%m-%d %H:%M:%S"),
                  DateEn = as.POSIXct(.data$EndDate, format="%Y-%m-%d %H:%M:%S"),
                  .keep="unused") |>



    ## renaming {PID}:
    do_if(unique_id=="PID",
          function(df) dplyr::rename(df, record_id=.data$PID) ) |>


    ## move columns to the front of the dataframe {PID or record_id}:
    do_if(unique_id %in% c("PID", "record_id"),
          function(df)  dplyr::relocate(df, dplyr::any_of( c("record_id", "DateSt", "DateEn",
                                                             "Finished", "Progress", "Duration") )) ) |>


    ## filter, add, and move {uvm}:
    do_if(unique_id == "uvmid+uvmSurveyID",
          function(df) {

            df |>

              dplyr::filter(!is.na(.data$uvmid) & !is.na(.data$uvmSurveyID)) |>          ## filter out id NAs

              dplyr::full_join(key_df, by="uvmid") |>                                    ## adding `record_id` (full_join keeps all rows)

              dplyr::relocate(dplyr::any_of( c("uvmSurveyID", "uvmid",                   ## move columns to the front of the dataframe
                                               "record_id", "DateSt", "DateEn",
                                               "Finished", "Progress", "Duration") ))

          } ) |>


    ## filter out id NAs {all}:
    dplyr::filter(!is.na(.data$record_id))



  # ## return value + + + + + + + + + + + + + +

  return(df)

  # ## + + + + + + + + + + + + + + + + + + + + +


}

