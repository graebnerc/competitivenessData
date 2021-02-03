source(here::here("R/helper_functions.R"))

#' Aggregates the own indicators on MIS and SGP
#'
#' @param countries_considered iso3c codes for the countries to be left in data
#' @param first_year first year considered
#' @param last_year last year considered
#' @return tibble with all indicators on sgp and mis manually assembled
setup_own_indicators <- function(
  countries_considered, first_year, last_year){
  # The MIS indicator
  mis_indicator <- data.table::fread(
    here::here("data-raw/mis_indicator.csv"),
    colClasses = c("character"), sep = ";", header = T)
  # mis_indicator <- dplyr::select(
  #   mis_indicator, c("Country", dplyr::starts_with("MIP")))

  # TODO What is "-" about? Missing value?
  mis_indicator <- dplyr::select(
    mis_indicator, "Country", dplyr::any_of(as.character(first_year:last_year)))

  mis_indicator <- tidyr::pivot_longer(
    data = mis_indicator,
    cols=-dplyr::one_of("Country"),
    names_to="year",
    values_to="MIP_status")

  mis_indicator <- dplyr::mutate(
    mis_indicator,
    year=as.double(gsub("MIP_", "", year)),
    MIP_status=as.factor(MIP_status),
    Country=countrycode::countrycode(Country, "eurostat", "iso3c"))


  # The SGP compliance indicator

  sgp_compliance_indicator <- data.table::fread(
    here::here("data-raw/sgp_compliance_indicator.csv"),
    header = TRUE, colClasses = "character", sep = ";")

  sgp_compliance_indicator <- dplyr::select(
    sgp_compliance_indicator,
    "Country", dplyr::any_of(as.character(first_year:last_year)))

  sgp_compliance_indicator <- tidyr::pivot_longer(
    data = sgp_compliance_indicator,
    cols=-dplyr::one_of("Country"),
    names_to="year",
    values_to="SGP_compliance")

  sgp_compliance_indicator <- dplyr::mutate(
    sgp_compliance_indicator,
    year=as.double(gsub("SGP_Compliance_", "", year)),
    SGP_compliance=as.factor(SGP_compliance),
    Country=countrycode::countrycode(Country, "eurostat", "iso3c"))

  # The SGP EDP indicator

  sgp_edp_indicator <- data.table::fread(
    here::here("data-raw/sgp_edp_indicator.csv"),
    header = TRUE, colClasses = "character", sep = ";")

  sgp_edp_indicator <- dplyr::select(
    sgp_edp_indicator,
    "Country", dplyr::any_of(as.character(first_year:last_year)))

  sgp_edp_indicator <- tidyr::pivot_longer(
    data = sgp_edp_indicator,
    cols=-dplyr::one_of("Country"),
    names_to="year",
    values_to="SGP_edp")

  sgp_edp_indicator <- dplyr::mutate(
    sgp_edp_indicator,
    year=as.double(gsub("SGP_Compliance_", "", year)),
    SGP_edp=as.factor(SGP_edp),
    Country=countrycode::countrycode(Country, "eurostat", "iso3c"))

  # Merging

  all_indicators <- dplyr::full_join(mis_indicator, sgp_compliance_indicator,
                                     by = c("year", "Country"))
  all_indicators <- dplyr::full_join(all_indicators, sgp_edp_indicator,
                                     by = c("year", "Country"))
  all_indicators <- dplyr::filter(all_indicators,
    year>=first_year, year<=last_year, Country %in% countries_considered)

  unique_data <- test_uniqueness(all_indicators, c("year", "Country"))
  if (unique_data){
    var_labels <- c(
      "year"="year",  #"year of observation",
      "Country"="geo", #"country code (iso3c), except aggregated areas",
      "MIP_status"="Status in MIP; own indicator",
      "SGP_compliance"="Compliance with SGP; own indicator",
      "SGP_edp"="Compliance with SGP 2; own indicator"
    )

    Hmisc::label(all_indicators) = as.list(
      var_labels[match(names(all_indicators), names(var_labels))])
    rel_path <- "data-raw/own-indicators_annual.rds"
    # saveRDS(all_indicators, file = here::here(rel_path))
    # print(paste0("Saved own indicators data to: ", rel_path))
  } else {
    stop("Own indicator data not unique. Please check manually!")
  }
  all_indicators
}
