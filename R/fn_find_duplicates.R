

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
#'
#' @importFrom rlang .data
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
        dplyr::select("uvmid", "record_id") |>
        dplyr::count(.data$uvmid, .data$record_id) |>
        dplyr::filter(.data$n>1) |>
        dplyr::mutate(di = .data$uvmid)

      ## keeping only the duplicated IDs
      df_dr <- df[df$uvmid %in% df_di$di,]


      ## multiple SurveyID values
    } else {

      ## finding duplicated survey/ID combinations
      df_di <- df |>
        dplyr::select("uvmSurveyID", "uvmid", "record_id") |>
        dplyr::group_by(.data$uvmSurveyID) |>
        dplyr::count(.data$uvmid, .data$record_id) |>
        dplyr::filter(.data$n>1) |>
        dplyr::ungroup() |>
        dplyr::mutate(di = paste(.data$uvmSurveyID, .data$uvmid, sep="_"))

      ## keeping only the duplicated combinations
      df_dr <- df[paste(df$uvmSurveyID, df$uvmid, sep="_") %in% df_di$di,]

    }

    ## sorting/ordering the rows
    df_dr_s <- df_dr |>
      dplyr::arrange(.data$uvmSurveyID, .data$uvmid, .data$record_id, dplyr::desc(.data$Finished), dplyr::desc(.data$Progress), dplyr::desc(.data$Duration))

    ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  } else if (unique_id == "record_id") {

    ## finding duplicated IDs
    df_di <- df |>
      dplyr::count(.data$record_id) |>
      dplyr::filter(.data$n>1) |>
      dplyr::mutate(di = .data$record_id)

    ## keeping only the duplicated IDs
    df_dr <- df[df$record_id %in% df_di$di,]

    ## sorting/ordering the rows
    df_dr_s <- df_dr |>
      dplyr::arrange(.data$record_id, dplyr::desc(.data$Finished), dplyr::desc(.data$Progress), dplyr::desc(.data$Duration))

  }
  ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


  ## returning the final data frame
  return(df_dr_s)


}

