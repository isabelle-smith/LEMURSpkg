

#' @title Flag or filter duplicated rows by columns.
#' @description
#' This function was made to be used after [LEMURSpkg::fn_check_duplicates()].
#'
#' @param df Data frame to check.
#' @param group_col String: name of column in input data frame that groups duplicate rows. Defaults to `group`.
#' @param sort_col String: name of column in input data frame containing dates. Defaults to `DateSt`.
#' @param keep One of `"first"` or `"last"`. Which row in a group to keep/flag, when sorted by `sort_col` (ascending). Defaults to `"first`.
#' @param flag_only Logical: whether or not to mark the desired rows without removing others. Defaults to `TRUE`.
#'
#' @returns Data frame with either:
#' 1. all original columns plus column `keep_one` and all original rows (if `flag_only=TRUE`)
#' 2. all original columns and only one row per duplicate group (if `flag_only=FALSE`)
#'
#' @export
#'
#' @examples
#' ## `fn_check_duplicates` output:
#' LEMURS_check_df <- LEMURSpkg::fn_check_duplicates(LEMURSpkg::LEMURS_dupe_df,
#'                                                   id_cols      = c("surveyID", "recordID"),
#'                                                   exclude_cols = c("progress", "variablR"),
#'                                                   later_cols   = c("variablC"),
#'                                                   orig_num="orig_row",
#'                                                   na_equal=FALSE,
#'                                                   return_new=FALSE)
#'
#' ## applying `fn_keep_one_attempt`:
#' LEMURS_keep_TRUE <- fn_keep_one_attempt(LEMURS_check_df,
#'                                         group_col="group",
#'                                         sort_col="rec_date") ## implied `keep="first"`
#'
#' ## using `flag_only=FALSE`:
#' LEMURS_keep_FALSE <- fn_keep_one_attempt(LEMURS_check_df,
#'                                          group_col="group",
#'                                          sort_col="rec_date",
#'                                          flag_only=FALSE)
#'
#' ## comparing output (should be TRUE):
#'
#' LEMURS_dupe_compare <- mapply(LEMURSpkg::fn_equal_with_na,
#'                               subset(LEMURS_keep_TRUE, keep_one==1, select=-keep_one),
#'                               LEMURS_keep_FALSE)
#'
#' if ( all(LEMURS_dupe_compare) ){
#'    paste("subset(LEMURS_dupe_kt, keep_one==1, select=-keep_one)",
#'          "is equal to",
#'          "LEMURS_dupe_kf")
#' }
#'


fn_keep_one_attempt <- function(df,
                                group_col="group",
                                sort_col="DateSt",
                                keep="first",
                                flag_only=TRUE){

  ## <<< update this to reflect the values `unique_id` can accept >>>
  valid_keep_values <- c("first","last")


  ## stop actions: ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  stopifnot(group_col %in% names(df))


  if ( !(keep %in% valid_keep_values) ) {

    err_message_keep <- paste("[fn_keep_one_attempt]\n",
                              "invalid `keep` value: \"", keep, "\"\n",
                              "please use one of: ",
                              paste(paste0("\"", valid_keep_values, "\""), collapse=" or "),
                              sep="")

    stop(err_message_keep)

  }


  if ( !(sort_col %in% names(df)) ) {

    err_message_date <- paste("[fn_keep_one_attempt]\n",
                              "column `", sort_col, "` not found\n",
                              "check `sort_col` value",
                              sep="")

    stop(err_message_date)

  }

  ## ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  if (flag_only & keep=="first"){

    edit_df <- df |>
      dplyr::group_by(!!!rlang::parse_exprs(group_col)) |>
      dplyr::arrange(!!!rlang::parse_exprs(sort_col)) |>
      dplyr::mutate(keep_one = dplyr::if_else(dplyr::row_number() == 1, 1L, 0L)) |>
      dplyr::ungroup()

  } else if (flag_only & keep=="last") {

    edit_df <- df |>
      dplyr::group_by(!!!rlang::parse_exprs(group_col)) |>
      dplyr::arrange(dplyr::desc(!!!rlang::parse_exprs(sort_col))) |>
      dplyr::mutate(keep_one = dplyr::if_else(dplyr::row_number() == 1, 1L, 0L)) |>
      dplyr::ungroup()

  } else if (!flag_only & keep=="first"){

    edit_df <- df |>
      dplyr::arrange(!!!rlang::parse_exprs(sort_col)) |>
      dplyr::distinct(!!!rlang::parse_exprs(group_col), .keep_all=T)

  } else if (!flag_only & keep=="last") {

    edit_df <- df |>
      dplyr::arrange(dplyr::desc(!!!rlang::parse_exprs(sort_col))) |>
      dplyr::distinct(!!!rlang::parse_exprs(group_col), .keep_all=T)

  }

  return(edit_df)

}
