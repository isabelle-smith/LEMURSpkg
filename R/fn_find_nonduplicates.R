

#' @title Find non-duplicate rows by ID.
#' @description
#' This function was made to be used after [LEMURSpkg::fn_read_qualtrics_data()].
#'
#' @param df Data frame to check.
#' @param id_type One of `"record_id"` or `"uvmid+uvmSurveyID"`. Column(s) specified must be present in `df`.
#'
#' @returns Data frame with all original columns and only non-duplicate rows. Adds column `orig_row_num` if not already present.
#'
#' @importFrom rlang .data
#' @export
#'
#' @seealso [LEMURSpkg::fn_find_duplicates()]
#'
#' @examples
#'
#' ## ADD EXAMPLES
#' ## put file(s) in `extdata` directory
#' ## name = LEMURS_full_df ???
#'


fn_find_nonduplicates <- function(df,
                               id_type) {


  ## <<< update this to reflect the values `id_type` can accept >>>
  valid_ids <- c("record_id", "uvmid+uvmSurveyID")


  ## adding row #s
  if (!("orig_row_num" %in% names(df))) { df <- cbind(orig_row_num=seq_along(nrow(df)), df) }


  ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  if( !(id_type %in% valid_ids) ) {

    err_message <- paste("[fn_find_nonduplicates]\n",
                         "Invalid `id_type` value. Please use one of: ",
                         paste(paste0("\"", valid_ids, "\""), collapse=" or "),
                         sep="")

    stop(err_message)


    ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  } else if (id_type=="uvmid+uvmSurveyID") {

    ## only 1 SurveyID value
    if (length(unique(df$uvmSurveyID))==1){

      ## finding duplicated IDs
      df_di <- df |>
        dplyr::select("uvmid", "record_id") |>
        dplyr::count(.data$uvmid, .data$record_id) |>
        dplyr::filter(.data$n==1) |>
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
        dplyr::filter(.data$n==1) |>
        dplyr::ungroup() |>
        dplyr::mutate(di = paste(.data$uvmSurveyID, .data$uvmid, sep="_"))

      ## keeping only the duplicated combinations
      df_dr <- df[paste(df$uvmSurveyID, df$uvmid, sep="_") %in% df_di$di,]

    }

    ## sorting/ordering the rows
    df_dr_s <- df_dr |>
      dplyr::arrange(.data$uvmSurveyID, .data$uvmid, .data$record_id)

    ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  } else if (id_type == "record_id") {

    ## finding duplicated IDs
    df_di <- df |>
      dplyr::count(.data$record_id) |>
      dplyr::filter(.data$n==1) |>
      dplyr::mutate(di = .data$record_id)

    ## keeping only the duplicated IDs
    df_dr <- df[df$record_id %in% df_di$di,]

    ## sorting/ordering the rows
    df_dr_s <- df_dr |>
      dplyr::arrange(.data$record_id)

  }
  ## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


  ## returning the final data frame
  return(as.data.frame(df_dr_s))


}

