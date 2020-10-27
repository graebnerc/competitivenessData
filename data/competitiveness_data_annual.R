source(here::here("data-raw/mis-indicators.R"))
source(here::here("data-raw/pcc-indicators.R"))
source(here::here("data-raw/sgp-mis-own-indicators.R"))
source(here::here("R/get_solt_functions.R"))
source(here::here("R/helper_functions.R"))

country_sample <- c(
  "BEL", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "ITA", "CYP", "LVA", "LTU",
  "LUX", "MLT", "NLD", "AUT", "PRT", "SVN", "SVK", "FIN", "BGR", "CZE", "DNK",
  "HRV", "HUN", "POL", "ROU", "SWE", "GBR", "NOR", "CHE", "TUR", "RUS", "USA",
  "CAN", "MEX", "BRA", "AUS", "NZL", "JPN", "CHN", "HKG", "KOR"
)

year_start <- 1994
year_end <- 2020

download_data_www <- TRUE

country_classification_jee <- list()
country_classification_jee[["Core"]] <- countrycode::countrycode(
  c("Austria", "Belgium", "Denmark", "Finland", "Germany", "Sweden"),
  "country.name", "iso3c")
country_classification_jee[["Catchup"]] <- countrycode::countrycode(
  c("Bulgaria", "Romania", "Czech Republic", "Estonia", "Latvia",
    "Lithuania", "Hungary", "Poland", "Slovenia", "Slovakia", "Croatia"),
  "country.name", "iso3c")
country_classification_jee[["Finance"]] <- countrycode::countrycode(
  c("Luxembourg", "Netherlands", "Malta", "Ireland"),
  "country.name", "iso3c")
country_classification_jee[["Periphery"]] <- countrycode::countrycode(
  c("Cyprus", "France", "Greece",
    "Italy", "Portugal", "Spain"),
  "country.name", "iso3c")

pcc_data <- setup_pcc(download_data = download_data_www)
pcc_data <- dplyr::mutate(pcc_data, year=as.double(year))
pcc_data <- dplyr::mutate(pcc_data, iso3c=as.character(iso3c))

mis_data <- setup_mis(download_data = download_data_www)
mis_data <- dplyr::mutate(mis_data, year=as.double(year))
mis_data <- dplyr::mutate(mis_data, iso3c=as.character(iso3c))

own_data <- setup_own_indicators()
own_data <- dplyr::mutate(own_data, year=as.double(year))
own_data <- dplyr::mutate(own_data, Country=as.character(Country))

gini_data <- setup_swiid(
  download_data = download_data_www, file_name = "swiid9_0.rda",
  countries_considered = country_sample,
  first_year = year_start, last_year = year_end)

wb_data <- get_worldbank(
  download_data = download_data_www, countries_considered = country_sample,
  first_year = year_start, last_year = year_end)

full_annual_data <- dplyr::full_join(
  pcc_data, mis_data, by=c("iso3c", "year"))

full_annual_data <- dplyr::full_join(
  full_annual_data, gini_data, by=c("iso3c", "year"))

full_annual_data <- dplyr::full_join(
  full_annual_data, own_data, by=c("iso3c"="Country", "year"))

full_annual_data <- dplyr::full_join(
  full_annual_data, wb_data, by=c("iso3c", "year"))

full_annual_data <- dplyr::select(full_annual_data,
                                  iso3c, year, dplyr::everything())
data.table::setDT(full_annual_data)
full_annual_data[, country_group_jee := ifelse(
  iso3c %in% country_classification_jee[["Core"]], "Core", ifelse(
    iso3c %in% country_classification_jee[["Periphery"]], "Periphery", ifelse(
      iso3c %in% country_classification_jee[["Catchup"]], "Catchup", ifelse(
        iso3c %in% country_classification_jee[["Finance"]], "Finance", NA
    ))))]

file_path <-  here::here("data/competitiveness_data_annual")

test_uniqueness(full_annual_data, index_vars = c("year", "iso3c"))

data.table::fwrite(
  full_annual_data, paste0(file_path, ".csv"))

saveRDS(full_annual_data, paste0(file_path, ".rds"))

writexl::write_xlsx(full_annual_data, paste0(file_path, ".xlsx"))
