

fn_read_qualtrics_data <- function(full_file_path,
                                   col_types_list=list(.default = "c"),
                                   filter_ids=c("uvmid uvmSurveyID","PID"),
                                   drop_cols=c(),
                                   num_vars=c(),
                                   needs_key=FALSE,
                                   key_df=NULL) {



  ## help function _ _ _ _ _ _ _ _ _ _ _ _ _ _

  ## post: https://www.reddit.com/r/rstats/s/NSb2eg6Cj5
  ## comment: https://www.reddit.com/r/rstats/comments/16vbzaf/comment/k2r6a4q/
  ## monad: https://youtu.be/bK-Tz-GLfOs?si=gSY2lh9CJQ2VMDTI

  do_if <- function(df, cond, f){
    if(cond) f(df) else df
  }

  ## _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _



  df_names <- read_csv(full_file_path,
                       col_names = FALSE,
                       n_max = 1,
                       progress = FALSE,
                       show_col_types = FALSE) |> as.vector(mode="character")

  #col_types = list(Species = "lidc", .default = col_double())



  ## full dataframe:
  df <- read_csv(full_file_path,
                 col_names = FALSE,
                 skip = 3,
                 col_types = col_types_list,
                 progress = FALSE,
                 show_col_types = FALSE) |>


    ## drop rows 1+2 (Qualtrics info):
    slice(-c(1,2)) |>


    ## drop columns (Qualtrics info)
    select(-c(Status, IPAddress,
              RecipientLastName, RecipientFirstName,
              RecipientEmail, ExternalReference,                           ## kept:
              LocationLatitude, LocationLongitude,                         ##       StartDate, EndDate, Progress, Duration..in.seconds., Finished,
              DistributionChannel, UserLanguage)) |>                       ##       RecordedDate, ResponseId, ResponseID, SurveyID, uvmid, uvmSurveyID


    ## rename to remove spaces/parentheses
    rename(Duration=`Duration (in seconds)`) |>


    ## drop any other columns:
    do_if(length(drop_cols)>0,
          function(df) select(df, !drop_cols) ) |>


    ## filter out id NAs:
    do_if(filter_ids=="uvmid uvmSurveyID",
          function(df) filter(df, !is.na(uvmid) & !is.na(uvmSurveyID)) ) |>
    do_if(filter_ids=="PID",
          function(df) filter(df, !is.na(PID)) ) |>


    ## change listed columns to numeric (from string):
    do_if(length(num_vars)>0,
          function(df) mutate(df, across(.cols=all_of(numvars_all), .fns=as.numeric)) ) |>


    ## change date columns from strings to POSIXct:
    do_if(format_dates,
          function(df) mutate(df,
                              DateSt=as.POSIXct(StartDate, format="%Y-%m-%d %H:%M:%S"),
                              DateEn=as.POSIXct(EndDate, format="%Y-%m-%d %H:%M:%S"),
                              .keep="unused") ) |>


    ## adding `record_id` (full_join keeps all rows):
    ## ADD MORE ERROR CHECKING HERE < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < <
    do_if(needs_key,
          function(df) full_join(df, key_df, by=join_by(uvmid)) ) |>


    ## renaming PID:
    do_if(filter_ids=="PID",
          function(df) rename(df, record_id=PID) ) |>


    ## move columns to the front of the dataframe:
    do_if(filter_ids=="uvmid uvmSurveyID",
          function(df) relocate(c(uvmSurveyID, record_id, uvmid, DateSt, DateEn, Finished, Progress, Duration)) ) |>
    do_if(filter_ids=="PID",
          function(df)  relocate(c(record_id, DateSt, DateEn, Finished, Progress, Duration)) )



  # ## return value + + + + + + + + + + + + + +

  return(df)

  # ## + + + + + + + + + + + + + + + + + + + + +


}

