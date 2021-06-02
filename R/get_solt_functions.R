#' Get data from SWIID 9 database
#'
#' Gets pre and post tax Ginis from the standardized world income inequality
#'  database provided by Solt. Since names of data files change whenever the
#'  data is updated one must download the raw data manualle. The `.rda` file
#'  from the zip should be used as an input to this functino.
#'
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param file_name The name of the `.rda` file containing the data.
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
setup_swiid <- function(download_data, file_name, countries_considered,
                        first_year, last_year){
  if (download_data){
    warning("SWIID data cannot be downloaded automatically!")
  }
  swiid_data <- paste0(here::here("data-raw", file_name))
  swiid_data <- load_rda(file = swiid_data, object_name = "swiid_summary")
  data.table::setDT(swiid_data)
  swiid_data <- swiid_data[
    !country %in% c("Czechoslovakia", "Kosovo", "Micronesia", "Yugoslavia")]
  swiid_data[, iso3c:=countrycode::countrycode(
    country, "country.name", "iso3c")]
  swiid_data[, year:=as.double(year)]
  swiid_data[, iso3c:=as.character(iso3c)]
  swiid_data[, c("country", "redist", "redist_after"):=NULL]
  swiid_data <- swiid_data[iso3c %in% countries_considered]
  swiid_data <- swiid_data[year >= first_year & year <= last_year]
  renames <- c(
    "gini_disp"="gini_post", "gini_disp_se"="gini_post_se",
    "gini_mkt"="gini_pre", "gini_mkt_se"="gini_pre_se",
    "abs_red"="gini_abs_red", "abs_red_se"="gini_abs_red_se",
    "rel_red"="gini_rel_red", "rel_red_se"="gini_red_red_se"
  )
  data.table::setnames(swiid_data, old = names(renames),
                       new = unname(renames))
  # TODO Add labels
  # For Russia and the Soviet Union, duplicated values exist between
  #  1988 and 1990. Mean values are used.
  dup_data <- swiid_data[duplicated(swiid_data, by = c("iso3c", "year"))]
  if (all(dup_data[["year"]] == 1988:1990) & unique(dup_data[["iso3c"]]) == "RUS"){
    swiid_data <- swiid_data[, lapply(.SD, mean), by=c("iso3c", "year")]
  }

  stopifnot(test_uniqueness(swiid_data, c("iso3c", "year")))
  return(swiid_data)
}

