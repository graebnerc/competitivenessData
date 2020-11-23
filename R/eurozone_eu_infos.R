#' Add info about EU and Eurozone entry
#'
#' Gets information about when a country has entered the EU and the Eurozone.
#'  The variables are the following:
#' @return A tibble with the abovementioned vaiables
get_euro_info <- function(){
  basic_data_file <- here::here("data-raw/eu_entry_data.csv")
  basic_data <- data.table::fread(basic_data_file)
  dplyr::mutate(
    basic_data,
    iso3c=countrycode::countrycode(iso3c, "country.name", "iso3c"),
    EurozoneEntry=as.double(EurozoneEntry),
    EUEntry=as.double(EUEntry)
    )
  var_labels <- list(
    "iso3c"="The iso3c country code",
    "EurozoneEntry"="The year the country entered the Eurozone; NA if the country hasnt entered yet.",
    "EUEntry"="The year the country entered the European Union.")
  Hmisc::label(basic_data) = as.list(
    var_labels[match(names(basic_data), names(var_labels))])
  basic_data
}
