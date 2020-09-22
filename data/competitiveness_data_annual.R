source(here::here("data-raw/mis-indicators.R"))
source(here::here("data-raw/pcc-indicators.R"))

download_data_www <- FALSE

pcc_data <- setup_pcc(download_data = download_data_www)
pcc_data <- dplyr::mutate(pcc_data, year=as.double(year))
pcc_data <- dplyr::mutate(pcc_data, iso3c=as.character(iso3c))

mis_data <- setup_mis(download_data = download_data_www)
mis_data <- dplyr::mutate(mis_data, year=as.double(year))
mis_data <- dplyr::mutate(mis_data, iso3c=as.character(iso3c))

full_annual_data <- dplyr::full_join(
  pcc_data, mis_data, by=c("iso3c", "year"))
full_annual_data <- dplyr::select(full_annual_data,
                                  iso3c, year, dplyr::everything())

file_path <-  here::here("data/competitiveness_data_annual.")

data.table::fwrite(
  full_annual_data, paste0(file_path, ".csv"))

saveRDS(full_annual_data, paste0(file_path, ".rds"))

writexl::write_xlsx(full_annual_data, paste0(file_path, ".xlsx"))
