source(here::here("R/data_settings.R"))
solt_data <- setup_swiid(
  download_data = F, file_name = "swiid9_0.rda",
  countries_considered = country_sample,
  first_year = year_start, last_year = year_end)

usethis::use_data(solt_data, overwrite = TRUE)
