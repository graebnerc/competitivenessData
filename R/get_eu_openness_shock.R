#' Adds data on openness shocks as in Graebner et al. (2020, JEE)
setup_openness_shocks <- function(){
  eu_open_shocks <- data.table::fread(
    here::here("data-raw/openness-shocks.csv"))
  eu_open_shocks <- dplyr::mutate(
    eu_open_shocks,
    Country=countrycode::countrycode(Country, "country.name", "iso3c")
    )
  eu_open_shocks <- dplyr::rename(eu_open_shocks, iso3c=Country)

  unique_data <- test_uniqueness(
    eu_open_shocks, index_vars = c("iso3c"))

  if (unique_data){
    var_labels <- c(
      "iso3c"="Country code (iso3c)",
      "Shock"="Institutional EU entry acc. to Graebner et al. (2020, JEE).",
      "EUEntry"="Entry into the European Union."
    )
    Hmisc::label(eu_open_shocks) = as.list(
      var_labels[match(names(eu_open_shocks), names(var_labels))])
  } else {
    stop("eu_open_shocks not unique!")
  }
  return(eu_open_shocks)
}
