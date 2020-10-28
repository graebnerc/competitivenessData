if (F){
  source(here::here("R/helper_functions.R"))
  source(here::here("R/rci-indicators.R"))

  download_data_www <- TRUE
  rci_data_annual <- setup_rci(download_data = download_data_www)

  file_path <-  here::here("data/rci_data_annual")

  data.table::fwrite(
    rci_data_annual, paste0(file_path, "csv"))
  saveRDS(rci_data_annual, paste0(file_path, ".rds"))
  writexl::write_xlsx(rci_data_annual, paste0(file_path, ".xlsx"))

  usethis::use_data(rci_data_annual, overwrite = T, version = 3)
}
