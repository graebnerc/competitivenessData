source(here::here("data-raw/mis-indicators.R"))
source(here::here("data-raw/pcc-indicators.R"))
source(here::here("data-raw/sgp-mis-own-indicators.R"))
source(here::here("R/get_oecd_functions.R"))

country_sample <- c(
  "BEL", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "ITA", "CYP", "LVA", "LTU",
  "LUX", "MLT", "NLD", "AUT", "PRT", "SVN", "SVK", "FIN", "BGR", "CZE", "DNK",
  "HRV", "HUN", "POL", "ROU", "SWE", "GBR", "NOR", "CHE", "TUR", "RUS", "USA",
  "CAN", "MEX", "BRA", "AUS", "NZL", "JPN", "CHN", "HKG", "KOR"
)

year_start <- 1994
year_end <- 2020

download_data_www <- TRUE

pcc_data <- setup_pcc(download_data = download_data_www)
pcc_data <- dplyr::mutate(pcc_data, year=as.double(year))
pcc_data <- dplyr::mutate(pcc_data, iso3c=as.character(iso3c))

mis_data <- setup_mis(download_data = download_data_www)
mis_data <- dplyr::mutate(mis_data, year=as.double(year))
mis_data <- dplyr::mutate(mis_data, iso3c=as.character(iso3c))

own_data <- setup_own_indicators()
own_data <- dplyr::mutate(own_data, year=as.double(year))
own_data <- dplyr::mutate(own_data, Country=as.character(Country))

full_annual_data <- dplyr::full_join(
  pcc_data, mis_data, by=c("iso3c", "year"))

full_annual_data <- dplyr::full_join(
  full_annual_data, own_data, by=c("iso3c"="Country", "year"))

full_annual_data <- dplyr::select(full_annual_data,
                                  iso3c, year, dplyr::everything())

file_path <-  here::here("data/competitiveness_data_annual")

data.table::fwrite(
  full_annual_data, paste0(file_path, ".csv"))

saveRDS(full_annual_data, paste0(file_path, ".rds"))

writexl::write_xlsx(full_annual_data, paste0(file_path, ".xlsx"))
