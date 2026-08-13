

#' @title Keep newest or oldest duplicated rows.
#' @description
#' This function was made to be used after [LEMURSpkg::fn_check_duplicates()].
#'
#' @param df Data frame to check.
#' @param id_cols Character vector of columns used to identify which rows to compare.
#' @param group_col String. Name of column in input data frame that groups duplicate rows. Defaults to `group`.
#' @param date_col String. Name of column in input data frame containing dates. Defaults to `DateSt`.
#' @param keep One of `"first"` or `"last"`. Which row ina group to keep, when sorted by date (ascending). Defaults to `"first`.
#'
#' @returns tbd
#'


fn_keep_one_attempt <- function(df,
                                id_cols,
                                group_col="group",
                                date_col="DateSt",
                                keep="first"){

  ## <<< update this to reflect the values `unique_id` can accept >>>
  valid_keep_values <- c("first","last")


  ## stop actions: ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  stopifnot(all(id_cols %in% names(df))) ## do I need `id_cols` at all?
  stopifnot(group_col %in% names(df))


  if ( !(keep %in% valid_keep_values) ) {

    err_message_keep <- paste("[fn_keep_one_attempt]\n",
                              "invalid `keep` value: \"", keep, "\"\n",
                              "please use one of: ",
                              paste(paste0("\"", valid_keep_values, "\""), collapse=" or "),
                              sep="")

    stop(err_message_keep)

  }


  if ( !(date_col %in% names(df)) ) {

    err_message_date <- paste("[fn_keep_one_attempt]\n",
                              "column `", date_col, "` not found\n",
                              "check `date_col` value",
                              sep="")

    stop(err_message_date)

  }

  ## ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  if (keep=="first"){
    edit_df <- df |>
      dplyr::arrange(!!!rlang::parse_exprs(date_col)) |>
      dplyr::distinct(!!!rlang::parse_exprs(group_col), .keep_all=T)

  } else if (keep=="last") {

    edit_df <- df |>
      dplyr::arrange(dplyr::desc(!!!rlang::parse_exprs(date_col))) |>
      dplyr::distinct(!!!rlang::parse_exprs(group_col), .keep_all=T)
  }

  return(edit_df)

}
