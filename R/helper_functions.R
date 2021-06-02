#' Test uniqueness of data table
#'
#' Tests whether a data.table has unique rows.
#'
#' @param data_table A data frame of data table of which uniqueness should
#'  be tested.
#' @param index_vars Vector of strings, which specify the columns of
#'  data_table according to which uniqueness should be tested
#'  (e.g. country and year).
#' @return TRUE if data_table is unique, FALSE and a warning if it is not.
#' @family update_dataset_helpers
#' @import data.table
test_uniqueness <- function(data_table, index_vars, print_pos=TRUE){
  data_table <- data.table::as.data.table(data_table)
  if (nrow(data_table)!=data.table::uniqueN(data_table, by = index_vars)){
    warning(paste0(
      "Rows in the data.table: ", nrow(data_table),
      ", rows in the unique data.table:",
      data.table::uniqueN(data_table, by = index_vars),
      ". Run df[duplicated(df, by = c('key1'))] to see dup rows."))
    return(FALSE)
  } else {
    if (print_pos){
      print(paste0("No duplicates in ", as.list(sys.call()[[2]])))
    }
    return(TRUE)
  }
}

#' Test whether columns of a data frame contain missing values
#'
#' @param data_ A data.frame-like object
#' @param test_vars Character vector with column names of `data_`
#' @family update_dataset_helpers
#' @import dplyr
test_missings <- function(data_, test_vars){
  name_df <- as.list(sys.call()[[2]])
  missing_stat <- apply(
    dplyr::select(data_, dplyr::all_of(test_vars)),
    MARGIN = 2, FUN = function(x) sum(is.na(x)))
  if (sum(missing_stat)==0){
    print(paste0("No missing values in columns ",
                 paste(names(missing_stat), collapse = ", "),
                 " of df '", name_df, "'!"))
    return(TRUE)
  } else {
    warning(paste0("In df '", name_df, "' there are missing values in columns ",
                   paste(names(missing_stat), collapse = ", "), " : ",
                   paste(missing_stat, collapse = ", "), "!"))
    return(FALSE)
  }
}

#' Loads an object from an rda file
#'
#' @param file Path to the `.rda` file
#' @param object_name The name of the object to be returned; can be NULL
#' @return If `object_name` is not NULL the object; if yes, a list of all
#'  objects in the `file`
load_rda <- function(file, object_name=NULL) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  if (is.null(object_name)){
    return_object <- ls(tmp)
  } else {
    return_object <- tmp[[object_name]]
  }
  return(return_object)
}

