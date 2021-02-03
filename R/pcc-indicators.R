# From the README:

# EA19: Belgium, Germany, Estonia, Greece, Spain, France, Ireland, Italy,
#   Cyprus, Latvia, Lithuania, Luxembourg, Netherlands, Malta, Austria,
#   Portugal, Slovenia, Slovakia, Finland.

# EU27: EA19 +
#   Bulgaria, Czech Republic, Denmark, Croatia, Hungary, Poland, Romania, Sweden.

# IC37: EU27 +
#   Australia, Canada, Japan, Mexico, New Zealand, Norway, Switzerland, Turkey,
#   United Kingdom, USA

# broad group (42): IC 37 + Brazil, China, Hong Kong, Korea, Russia.

# BASE YEAR FOR CALCULATIONS : 2010=100
# FIRST CHAR.    A        Annual     from 1994 onwards
# Q        Quarterly  from 1994 Q1 onwards
# M        Monthly    from Jan 1994 onwards

# SECOND AND THIRD CHAR.
# 19: each member of gr42 compared to the Euro area (EA19)
# 27: each member of gr42 and 1 aggregate compared to the European Union (EU27)
# 37: each member of gr42 and 2 aggregates compared to rest of IC37
# 42: each member of the broad group (42) and 2 aggregates compared to the
#     rest of the group (for gr42: only HICP/CPI deflated REER available)


# LAST CHARACTERS
# NEER     Nominal Effective Exchange Rate
# REER     Real Effective Exchange Rate Nominal unit labour cost, total economy by deflator
# Price deflator GDP, market prices
# Price deflator exports of goods/services
# Price deflator HICP / CPI

# WGHTS    Weight matrices

source(here::here("R/helper_functions.R"))
#' Setup data from the quaterly report on price and cost competitiveness
#'
#' The data is a relative index with 2010=100. The function only considers
#'  annual data, quaterly and monthly data is not considered.
#'  The data contains information about the relative position of the countries.
#'  The comparison groups are the following:
#'  EA19: Belgium, Germany, Estonia, Greece, Spain, France, Ireland, Italy,
#'      Cyprus, Latvia, Lithuania, Luxembourg, Netherlands, Malta, Austria,
#'      Portugal, Slovenia, Slovakia, Finland.
#'  EU27: EA19 +
#'      Bulgaria, Czech Republic, Denmark, Croatia, Hungary, Poland,
#'      Romania, Sweden.
#'  IC37: EU27 +
#'      Australia, Canada, Japan, Mexico, New Zealand, Norway, Switzerland,
#'      Turkey, United Kingdom, USA
#'  More information about the variables can be obtained via the labels. The
#'  latter can be accessed via `View()` or `Hmisc::label()`.
#'  The country codes are iso3c codes except the two aggregated regions
#'  'Euro_Area' and 'European_Union', corresponding to groups EA19 and EU27.
setup_pcc <- function(download_data){
  raw_file <- here::here("data-raw/pcc-indicators.zip")
  if (download_data){
    print("Download PCC data.")
    url_competitiveness <- "https://ec.europa.eu/economy_finance/db_indicators/competitiveness/documents/stat_csv.zip"
    download.file(url = url_competitiveness, destfile = raw_file)
  } else{
    if (file.exists(raw_file)){
      print("Use previously downloaded PCC data.")
    } else{
      stop("Data does not exist locally. Download with `download_data=TRUE`.")
    }
  }
  # Extracted from the README:
  country_names <- data.table::fread(
    here::here("data-raw/pcc-indicators-countrycodes.csv"),
    colClasses = rep("character", 2), data.table = FALSE)

  comp_keys <- c(
    "19"="EA19",
    "27"="EU27",
    "37"="IC37",
    "42"="Gr42"
  )

  # Order the files
  all_files <- unzip(raw_file, list = TRUE)[["Name"]]
  annual_files <- all_files[grep("^A", all_files)]
  quaterly_files <- all_files[grep("^Q", all_files)]
  monthly_files <- all_files[grep("^M", all_files)]

  annual_noW_files <- annual_files[substr(annual_files, 4,4)!="W"]
  # These are all of the following:
  annual_RULC_files <- annual_files[grep("RULC", ignore.case = FALSE, annual_files)]
  annual_NEER_files <- annual_files[grep("NEER", ignore.case = FALSE, annual_files)]
  annual_RGDP_files <- annual_files[grep("RGDP", ignore.case = FALSE, annual_files)]
  annual_RHICP_files <- annual_files[grep("RHICP", ignore.case = FALSE, annual_files)]
  annual_RXPI_files <- annual_files[grep("RXPI", ignore.case = FALSE, annual_files)]
  # There are no REER files for annual case (only for matrices)
  # TODO: check whether monthly and quaterly data is useful

  data_frames <- list()
  for (f in 1:length(annual_noW_files)){
    current_file <- annual_noW_files[f]
    current_indicator <- substr(current_file, 4, nchar(current_file)-4)
    comp_base <- unname(comp_keys[substr(current_file, 2, 3)])
    indicator_name <- paste0(current_indicator, "_", comp_base)
    print(indicator_name)

    current_data <- data.table::fread(
      unzip(raw_file, files = current_file), header = T)
    names(current_data)[1] <- "year"

    current_data <- tidyr::pivot_longer(
      current_data,
      cols=-1,
      names_to = "country",
      values_to=indicator_name)

    current_data <- dplyr::mutate(
      current_data,
      country=countrycode::countrycode(
        sourcevar = country,
        origin = "num.code",
        destination = "country.name",
        custom_dict = country_names)
    )
    data_frames[[as.character(f)]] <- current_data
    unlink(current_file)
  }

  full_data <- purrr::reduce(
    data_frames,
    dplyr::full_join,
    by=c("year", "country")
  )
  full_data <-  dplyr::filter(
    full_data,
    !country %in% c("Euro_Area", "European_Union")
    )
  full_data <- dplyr::mutate(
    full_data, iso3c=countrycode::countrycode(
      country, "country.name", "iso3c"))
  full_data <- dplyr::mutate(full_data,
                             iso3c=ifelse(is.na(iso3c), country, iso3c))
  full_data <- dplyr::select(full_data, -country)

  unique_data <- test_uniqueness(full_data, c("year", "iso3c"))
  if (unique_data){
    var_labels <- c(
      "year"="time",  #"year of observation",
      "iso3c"="geo", #"country code (iso3c), except aggregated areas",
      "NEER_EA19"="Nominal Effective Exchange Rate, relative against Euro Area (2010=100)",
      "RGDP_EA19"="Price deflator GDP, market prices (?), relative against Euro Area (2010=100)",
      "RHICP_EA19"="Price deflator HICP / CPI, relative against Euro Area (2010=100)",
      "RULC_EA19"="Relative Unit Labor Costs, relative against Euro Area (2010=100)",
      "RXPI_EA19"="Price deflator exports of goods/services, relative against Euro Area (2010=100)",
      "NEER_EU27"="Nominal Effective Exchange Rate, relative against European Union of the 27 (2010=100)",
      "RGDP_EU27"="Price deflator GDP, market prices (?), relative against European Union of the 27 (2010=100)",
      "RHICP_EU27"="Price deflator HICP / CPI, relative against European Union of the 27 (2010=100)",
      "RULC_EU27"="Relative Unit Labor Costs, relative against European Union of the 27 (2010=100)",
      "RXPI_EU27"="Price deflator exports of goods/services, relative against European Union of the 27 (2010=100)",
      "NEER_IC37"="Nominal Effective Exchange Rate, relative against IC37 group (2010=100)",
      "RGDP_IC37"="Price deflator GDP, market prices (?), relative against IC37 group (2010=100)",
      "RHICP_IC37"="Price deflator HICP / CPI, relative against IC37 group (2010=100)",
      "RULC_IC37"="Relative Unit Labor Costs, relative against IC37 group (2010=100)",
      "RXPI_IC37"="Price deflator exports of goods/services, relative against IC37 group (2010=100)",
      "NEER_Gr42"="Nominal Effective Exchange Rate, relative against Gr42 group (2010=100)",
      "RGDP_Gr42"="Price deflator GDP, market prices (?), relative against Gr42 group (2010=100)",
      "RHICP_Gr42"="Price deflator HICP / CPI, relative against Gr42 group (2010=100)"
    )

    Hmisc::label(full_data) = as.list(
      var_labels[match(names(full_data), names(var_labels))])
    rel_path <- "data-raw/pcc-indicators_annual.rds"
    saveRDS(full_data, file = here::here(rel_path))
    print(paste0("Saved PCC data to: ", rel_path))
  } else {
    stop("Annual PCC data not unique. Please check manually!")
  }
  return(full_data)
}
