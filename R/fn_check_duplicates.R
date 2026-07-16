

#' @title Compare values of duplicate rows.
#'
#' @description
#' Description goes here.
#'
#' @param df Data frame to check.
#' @param id_cols Character vector of columns used to identify which rows to compare.
#' @param exclude_cols Optional. Character vector of columns left out entirely.
#' @param later_cols Optional. Character vector of columns only compared if rows are otherwise identical.
#' @param na_equal Logical. Whether or not a value should be considered identical to NA. Defaults to `FALSE`.
#' @param return_new Logical. Whether or not to return a `"new"` data frame (vs. the input data frame with columns added). Defaults to `TRUE`.
#'
#' @returns Input or new data frame with columns is_empty (boolean), status (string), matched_rows (string), and one or more <col>_match (boolean).
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' fn_check_duplicates(LEMURSpkg::LEMURS_dupe_df,
#'   id_cols      = c("surveyID", "recordID"),
#'   exclude_cols = c("progress", "variablR"),
#'   later_cols   = c("variablC"))



fn_check_duplicates <- function(df,
                                id_cols,
                                exclude_cols=c(),
                                later_cols=c(),
                                na_equal=NA,
                                return_new=TRUE) {

  stopifnot(all(id_cols %in% names(df)))
  stopifnot(all(exclude_cols %in% names(df)))
  stopifnot(all(later_cols %in% names(df)))


  compare_cols <- base::setdiff(colnames(df), c(id_cols, exclude_cols, later_cols))


  # ---- helpers ---------------------------------------------------------

  # NA-aware equality: NA == NA is TRUE, and...
  if (na_equal) {                                   ## NA vs a value is TRUE
    val_equal <- function(a, b) {
      if (is.na(a) && is.na(b)) return(TRUE)
      if (is.na(a) || is.na(b)) return(TRUE)
      return(a == b)
    }

  } else {                                         ## NA vs a value is FALSE (mismatch)
    val_equal <- function(a, b) {
      if (is.na(a) && is.na(b)) return(TRUE)
      if (is.na(a) || is.na(b)) return(FALSE)
      return(a == b)
    }

  }

  # a row is "empty" if every comparison column is NA
  row_is_empty <- function(i) {
    all(vapply(compare_cols, function(cn) is.na(df[[cn]][i]), logical(1)))
  }

  # compare row i to row j across compare_cols -> named logical vector
  compare_rows <- function(i, j) {
    vapply(compare_cols, function(cn) val_equal(df[[cn]][i], df[[cn]][j]), logical(1))
  }


  # ---- pass 1: flag empty rows -----------------------------------------
  df$is_empty <- vapply(seq_len(nrow(df)), row_is_empty, logical(1))

  # ---- pass 2: classify every row against its group ---------------------
  group_key <- do.call(interaction, c(df[id_cols], drop = TRUE))
  groups    <- split(seq_len(nrow(df)), group_key)

  status       <- character(nrow(df))
  matched_rows <- vector("list", nrow(df))

  # one match-flag column per later_col, e.g. variablC_match
  later_match <- as.data.frame(
    matrix(NA, nrow = nrow(df), ncol = length(later_cols),
           dimnames = list(NULL, paste0(later_cols, "_match")))
  )

  for (idxs in groups) {

    for (i in idxs) {
      others <- dplyr::setdiff(idxs, i)

      if (df$is_empty[i]) {
        status[[i]] <- "empty"
        next
      }

      if (length(others) == 0) {
        status[[i]] <- "singleton"
        next
      }

      other_empty <- vapply(others, function(j) df$is_empty[j], logical(1))

      if (all(other_empty)) {
        status[[i]] <- "all_other_rows_empty"
        next
      }

      nonempty_others <- others[!other_empty]
      eq_list <- lapply(nonempty_others, function(j) compare_rows(i, j))
      names(eq_list) <- nonempty_others

      identical_with <- nonempty_others[vapply(eq_list, all, logical(1))]
      partial_with   <- nonempty_others[vapply(eq_list, function(e) any(e) && !all(e), logical(1))]

      if (length(identical_with) > 0) {
        status[[i]]      <- "completely_identical"
        matched_rows[[i]] <- identical_with

        if (length(later_cols) > 0) {
          for (cn in later_cols) {
            later_match[i, paste0(cn, "_match")] <-
              all(vapply(identical_with, function(j) val_equal(df[[cn]][i], df[[cn]][j]), logical(1)))
          }
        }
      } else if (length(partial_with) > 0) {
        status[[i]]      <- "partially_identical"
        matched_rows[[i]] <- partial_with
      } else {
        status[[i]] <- "no_match"
      }
    }
  }


  ## starting results df
  results <- df[, c(id_cols, "is_empty")]


  # ---- assemble results --------------------------------------------------
  results$status       <- status
  results$matched_rows <- sapply(matched_rows, function(x) if (length(x) == 0) NA_character_ else paste(x, collapse = ","))
  results <- cbind(results, later_match)
  results$row_num <- seq_len(nrow(results))

  ## adding result columns to input (`return_new` = FALSE)
  if (!return_new) {
    results <- cbind(results,
                     df[base::setdiff(names(df), c(id_cols, "is_empty"))])
  }

  return(results)

}

