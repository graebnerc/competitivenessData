#' Gets data on SGP from Eurostat
#'
#' Collects data from Eurostat on SGP indicators, i.e. on government deficit
#'  and total government debt.
#'
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
get_eurostat_sgp <- function(download_data, countries_considered,
                             first_year=1995, last_year=2020){

  # Defitics:
  file_name_deficit <- here::here("data-raw/eurostat_gov_10dd_edpt1.csv")
  if (download_data){
    filter_list_1 <- list(
      sector = c("S13"), # S13 General governmen; S1: Total Economy
      na_item = c("B9"), # B9: Net lending (+) /net borrowing (-)
      unit = c("PC_GDP") # PC_GDP: percent of GDP
    )

    gov_deficit <- eurostat::get_eurostat(
      "gov_10dd_edpt1", filters = filter_list_1, time_format = "num")
    gov_deficit <- dplyr::select(
      gov_deficit, dplyr::all_of(c("geo", "time", "values")))

    data.table::fwrite(
      gov_deficit, file = file_name_deficit)

  } else if(file.exists(file_name_deficit)) {
    gov_deficit <- data.table::fread(
      file_name_deficit,
      colClasses = c("character", "double", "double"))
  } else {
    stop("eurostat_gov_10dd_edpt1.csv doesn't exist and must be be downloaded.")
  }

  gov_deficit <- dplyr::rename(
    gov_deficit, iso3c=geo, year=time, gov_balance=values)
  gov_deficit <- dplyr::mutate(
    gov_deficit, iso3c=countrycode::countrycode(
      iso3c, "eurostat", "iso3c", warn = F))
  gov_deficit <- dplyr::filter(gov_deficit, !is.na(iso3c),
                               year<=last_year, year>=first_year)

  # Total debt:
  file_name_debt <- here::here("data-raw/eurostat_tipsgo10.csv")
  if (download_data){
    filter_list_2 <- list(
      sector = c("S13"), # S13 General government
      na_item=c("GD"), # GD: Government debt
      unit=c("PC_GDP") # PC_GDP: percent of GDP
    )

    gov_debt <- eurostat::get_eurostat(
      "tipsgo10", filters = filter_list_2, time_format = "num")
    gov_debt <- dplyr::select(
      gov_debt, dplyr::all_of(c("geo", "time", "values")))

    data.table::fwrite(
      gov_debt, file = file_name_debt)
  } else if(file.exists(file_name_debt)) {
    gov_debt <- data.table::fread(
      file_name_debt,
      colClasses = c("character", "double", "double"))
  } else {
    stop("eurostat_tipsgo10.csv doesn't exist and must be be downloaded.")
  }

  gov_debt <- dplyr::rename(
    gov_debt, iso3c=geo, year=time, gov_debt_gdp=values)
  gov_debt <- dplyr::mutate(
    gov_debt, iso3c=countrycode::countrycode(
      iso3c, "eurostat", "iso3c", warn = F))
  gov_debt <- dplyr::filter(gov_debt, !is.na(iso3c),
                            year<=last_year, year>=first_year)

  # Merging
  sgp_data <- dplyr::full_join(gov_deficit, gov_debt, by=c("iso3c", "year"))

  unique_data <- test_uniqueness(
    sgp_data, index_vars = c("iso3c", "year"))

  if (unique_data){
    var_labels <- c(
      "iso3c"="Country code (iso3c)",
      "gov_debt_gdp"="Government debt in % of GDP as used in SGP rules.",
      "gov_balance"="Government balance in % of GDP as used in SGP rules."
    )
    Hmisc::label(sgp_data) = as.list(
      var_labels[match(names(sgp_data), names(var_labels))])
  } else {
    stop("sgp_data not unique!")
  }
  return(sgp_data)
}
