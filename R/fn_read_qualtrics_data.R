

fn_read_qualtrics_data <- function(full_file_path,
                                   col_types_list=list(.default = "c"),
                                   filter_ids=c("PID", "record_id", "uvmid uvmSurveyID"),
                                   drop_cols=c(),
                                   num_vars=c(),
                                   key_df=NULL) {



  ## helper funcs _ _ _ _ _ _ _ _ _ _ _ _ _ _

  ## post: https://www.reddit.com/r/rstats/s/NSb2eg6Cj5
  ## comment: https://www.reddit.com/r/rstats/comments/16vbzaf/comment/k2r6a4q/
  ## monad: https://youtu.be/bK-Tz-GLfOs?si=gSY2lh9CJQ2VMDTI

  do_if <- function(df, cond, f){
    if(cond) f(df) else df
  }

  ## _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _



  ## column names only...
  df_names <- read_csv(full_file_path,
                       col_names = FALSE,
                       n_max = 1,
                       progress = FALSE,
                       show_col_types = FALSE) |> as.vector(mode="character")



  ## full dataframe...
  df <- read_csv(full_file_path,
                 col_names = FALSE,
                 skip = 3,
                 col_types = col_types_list,
                 progress = FALSE,
                 show_col_types = FALSE) |>


    ## drop rows 1+2 (Qualtrics info) {all}:
    slice(-c(1,2)) |>


    ## drop columns (Qualtrics info) {all}
    select(-c(Status, IPAddress,                             ## kept: StartDate, EndDate,
              RecipientLastName, RecipientFirstName,         ##       Progress, Duration..in.seconds., Finished,
              RecipientEmail, ExternalReference,             ##       RecordedDate, ResponseId, ResponseID, SurveyID
              LocationLatitude, LocationLongitude,           ##
              DistributionChannel, UserLanguage)) |>         ## also: uvmid, uvmSurveyID / PID / SC0, Score / etc.


    ## rename to remove spaces/parentheses {all}
    rename(Duration = `Duration (in seconds)`) |>


    ## drop any other columns:
    do_if(length(drop_cols) > 0,
          function(df) select(df, !drop_cols) ) |>


    ## change listed columns to numeric (from string):
    do_if(length(num_vars) > 0,
          function(df) mutate(df, across(.cols=all_of(numvars_all), .fns=as.numeric)) ) |>


    ## change date columns from strings to POSIXct {all}:
    mutate(DateSt = as.POSIXct(StartDate, format="%Y-%m-%d %H:%M:%S"),
           DateEn = as.POSIXct(EndDate, format="%Y-%m-%d %H:%M:%S"),
           .keep="unused") |>



    ## renaming {PID}:
    do_if(filter_ids=="PID",
          function(df) rename(df, record_id=PID) ) |>


    ## move columns to the front of the dataframe {PID or record_id}:
    do_if(filter_ids %in% c("PID", "record_id"),
          function(df)  relocate(df, c(record_id, DateSt, DateEn, Finished, Progress, Duration)) ) |>


    ## filter, add, and move {uvm}:
    do_if(filter_ids == "uvmid uvmSurveyID",
          function(df) {

            df |>

              filter(!is.na(uvmid) & !is.na(uvmSurveyID)) |>     ## filter out id NAs

              full_join(df, key_df, by=join_by(uvmid)) |>        ## adding `record_id` (full_join keeps all rows)

              relocate(df, c(uvmSurveyID, record_id, uvmid,
                             DateSt, DateEn,
                             Finished, Progress, Duration))      ## move columns to the front of the dataframe

            } ) |>


    ## filter out id NAs {all}:
    filter(!is.na(record_id))



  # ## return value + + + + + + + + + + + + + +

  return(df)

  # ## + + + + + + + + + + + + + + + + + + + + +


}

