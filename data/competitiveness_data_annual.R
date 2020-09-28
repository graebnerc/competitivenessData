source(here::here("data-raw/mis-indicators.R"))
source(here::here("data-raw/pcc-indicators.R"))
source(here::here("data-raw/sgp-mis-own-indicators.R"))

download_data_www <- FALSE

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
