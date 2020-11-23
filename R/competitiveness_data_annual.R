#' Rebuild the competitiveness data set
#'
#' Function used to create the data set.
build_annual_competitiveness <- function(
  download_data_www = FALSE,
  year_start=1994, year_end=2020){

  source(here::here("R/data_settings.R"))
  pcc_data <- setup_pcc(download_data = download_data_www)
  pcc_data <- dplyr::mutate(pcc_data, year=as.double(year))
  pcc_data <- dplyr::mutate(pcc_data, iso3c=as.character(iso3c))

  mis_data <- setup_mis(download_data = download_data_www)
  mis_data <- dplyr::mutate(mis_data, year=as.double(year))
  mis_data <- dplyr::mutate(mis_data, iso3c=as.character(iso3c))

  desc_data <- get_euro_info()
  desc_data <- dplyr::mutate(desc_data, iso3c=as.character(iso3c))

  own_data <- setup_own_indicators(
    countries_considered = country_sample,
    first_year = year_start, last_year = year_end)
  own_data <- dplyr::mutate(own_data, year=as.double(year))
  own_data <- dplyr::mutate(own_data, Country=as.character(Country))

  gini_data <- setup_swiid(
    download_data = download_data_www, file_name = "swiid9_0.rda",
    countries_considered = country_sample,
    first_year = year_start, last_year = year_end)

  wb_data <- get_worldbank(
    download_data = download_data_www, countries_considered = country_sample,
    first_year = year_start, last_year = year_end)

  csr_data <- setup_imp_scores()
  csr_data <- dplyr::mutate(csr_data, year=as.double(year))
  csr_data <- dplyr::mutate(csr_data, iso3c=as.character(iso3c))

  full_annual_data <- dplyr::full_join(
    pcc_data, mis_data, by=c("iso3c", "year"))

  full_annual_data <- dplyr::full_join(
    pcc_data, desc_data, by=c("iso3c"))

  full_annual_data <- dplyr::full_join(
    full_annual_data, gini_data, by=c("iso3c", "year"))

  full_annual_data <- dplyr::full_join(
    full_annual_data, own_data, by=c("iso3c"="Country", "year"))

  full_annual_data <- dplyr::full_join(
    full_annual_data, wb_data, by=c("iso3c", "year"))

  full_annual_data <- dplyr::full_join(
    full_annual_data, csr_data, by=c("iso3c", "year"))

  full_annual_data <- dplyr::select(full_annual_data,
                                    iso3c, year, dplyr::everything())
  data.table::setDT(full_annual_data)

  country_classification_jee <- list(
    "Core" = countrycode::countrycode(
      c("Austria", "Belgium", "Denmark", "Finland", "Germany", "Sweden"),
      "country.name", "iso3c"),
    "Catchup" = countrycode::countrycode(
      c("Bulgaria", "Romania", "Czech Republic", "Estonia", "Latvia",
        "Lithuania", "Hungary", "Poland", "Slovenia", "Slovakia", "Croatia"),
      "country.name", "iso3c"),
    "Finance" = countrycode::countrycode(
      c("Luxembourg", "Netherlands", "Malta", "Ireland"),
      "country.name", "iso3c"),
    "Periphery" = countrycode::countrycode(
      c("Cyprus", "France", "Greece",
        "Italy", "Portugal", "Spain"),
      "country.name", "iso3c")
  )

  full_annual_data[, country_group_jee := ifelse(
    iso3c %in% country_classification_jee[["Core"]], "Core", ifelse(
      iso3c %in% country_classification_jee[["Periphery"]], "Periphery", ifelse(
        iso3c %in% country_classification_jee[["Catchup"]], "Catchup", ifelse(
          iso3c %in% country_classification_jee[["Finance"]], "Finance", NA
        ))))]

  test_uniqueness(full_annual_data, index_vars = c("year", "iso3c"))

  full_annual_data
}

# The following lines are used to re-create the data.
# It should not be executed when the package is built but is only for explicit
# use when maintaining the package.

if (F){
  source(here::here("R/mis-indicators.R"))
  source(here::here("R/pcc-indicators.R"))
  source(here::here("R/get_worldbank_functions.R"))
  source(here::here("R/sgp-mis-own-indicators.R"))
  source(here::here("R/get_solt_functions.R"))
  source(here::here("R/CSR_database.R"))
  source(here::here("R/helper_functions.R"))
  source(here::here("R/eurozone_eu_infos.R"))

  competitiveness_data_macro <- build_annual_competitiveness()

  file_path <-  here::here("data/competitiveness_data_macro")
  data.table::fwrite(
    competitiveness_data_macro, paste0(file_path, ".csv"))
  saveRDS(competitiveness_data_macro, paste0(file_path, ".rds"))
  writexl::write_xlsx(competitiveness_data_macro, paste0(file_path, ".xlsx"))

  usethis::use_data(competitiveness_data_macro, overwrite = T, version = 3)
}
