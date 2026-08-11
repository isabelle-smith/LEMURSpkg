

#' @title Kepp most recent duplicated rows.
#' @description
#' This function was made to be used after [LEMURSpkg::fn_check_duplicates()].
#'
#' @param df Data frame to check.
#' @param id_cols Character vector of columns used to identify which rows to compare.
#' @param date_col String. Name of column in input data frame containing dates. Defaults to `DateSt`.
#' @param keep One of `"first"` or `"last"`. Which ????
#'
#' @returns tbd
#'


fn_keep_one_attempt <- function(df,
                                id_cols,
                                date_col="DateSt",
                                keep="first"){

  ## stop actions: ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  stopifnot(all(id_cols %in% names(df)))

  if ( !(date_col %in% names(df)) ) {

    err_message_date <- cat("column `", date_col, "` not found\n",
                            "check `date_col` value",
                            sep="")

    stop(err_message_date)

  }

  ## ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


}
