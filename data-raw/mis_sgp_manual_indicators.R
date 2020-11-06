source(here::here("R/data_settings.R"))

mis_sgp_manual_indicators <- setup_own_indicators(
  countries_considered = country_sample,
  first_year = year_start, last_year = year_end)

usethis::use_data(mis_sgp_manual_indicators, overwrite = TRUE)
