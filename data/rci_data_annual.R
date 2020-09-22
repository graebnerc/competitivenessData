source(here::here("data-raw/rci-indicators.R"))

download_data_www <- TRUE

rci_data <- setup_rci(download_data = download_data_www)

data.table::fwrite(
  rci_data, here::here("data/rci_data_annual.csv"))

saveRDS(rci_data, here::here("data/rci_data_annual.rds"))
