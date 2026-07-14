

#' @title Find duplicate rows by ID.
#'
#' @description
#' Description goes here.
#'
#'
#' @param df Data frame to check.
#' @param unique_id One of "record_id" or "uvmid+uvmSurveyID". Column(s) specified must be present in `df`.
#'
#' @returns Data frame only duplicate rows but all columns.
#' @export
#'
#' @examples
#' ## ADD EXAMPLES
#' ## put file(s) in `extdata` directory
#' ## name = LEMURS_full_df ???


fn_find_duplicates <- function(df,
                               unique_id) {


  valid_ids <- c("record_id", "uvmid+uvmSurveyID")

  ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  if( !(unique_id %in% valid_ids) ) {

    err_message <- paste("Invalid `unique_id` value. Please use one of:",
                         paste(paste0("`", valid_ids, "`"), collapse=" or "))

    stop(err_message)


    ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  } else if (unique_id=="uvmid+uvmSurveyID") {

    ## only 1 SurveyID value
    if (length(unique(df$uvmSurveyID))==1){

      ## finding duplicated IDs
      df_di <- df |>
        select(uvmid, record_id) |>
        count(uvmid, record_id) |>
        filter(n>1) |>
        mutate(di = uvmid)

      ## keeping only the duplicated IDs
      df_dr <- df[df$uvmid %in% df_di$di,]


    ## multiple SurveyID values
    } else {

      ## finding duplicated survey/ID combinations
      df_di <- df |>
        select(uvmSurveyID, uvmid, record_id) |>
        group_by(uvmSurveyID) |>
        count(uvmid, record_id) |>
        filter(n>1) |>
        ungroup() |>
        mutate(di = paste(uvmSurveyID, uvmid, sep="_"))

      ## keeping only the duplicated combinations
      df_dr <- df[paste(df$uvmSurveyID, df$uvmid, sep="_") %in% df_di$di,]

    }

    ## sorting/ordering the rows
    df_dr_s <- df_dr |>
      arrange(uvmSurveyID, uvmid, record_id, desc(Finished), desc(Progress), desc(Duration))

    ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  } else if (unique_id == "record_id") {

    ## finding duplicated IDs
    df_di <- df |>
      count(record_id) |>
      filter(n>1) |>
      mutate(di = record_id)

    ## keeping only the duplicated IDs
    df_dr <- df[df$record_id %in% df_di$di,]

    ## sorting/ordering the rows
    df_dr_s <- df_dr |>
      arrange(record_id, desc(Finished), desc(Progress), desc(Duration))

  }
  ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


  ## returning the final data frame
  return(df_dr_s)


}

