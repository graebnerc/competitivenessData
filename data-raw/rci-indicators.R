source(here::here("R/helper_functions.R"))

setup_rci <- function(download_data){
  raw_file_change <- here::here("data-raw/rci-indicator-evolution_raw.csv")
  if (download_data){
    print("Download RCI data!")

    rci_changes_api <- "https://cohesiondata.ec.europa.eu/api/views/geur-y3df/rows.csv?accessType=DOWNLOAD"
    download.file(url = rci_changes_api, destfile = raw_file_change)
  } else{
    if (file.exists(raw_file_change)){
      print("Use previously downloaded RCI data.")
    } else{
      stop("RCI data does not exist locally. Download with `download_data=TRUE`.")
    }
  }
  rci_raw <- data.table::fread(raw_file_change)
  rci_data <- dplyr::select(rci_raw, -dplyr::one_of("Year z-score"))
  rci_data <- dplyr::mutate(
    rci_data, Country=countrycode::countrycode(Country, "country.name", "iso3c"))
  names(rci_data) <- make.names(names(rci_data), allow_ = FALSE)

  rci_data <- dplyr::rename(
    rci_data,
    iso3c=Country,
    year=Year,
    rci_value2019=value,
    rci_2019_vs_2016=X2019.vs.2016,
    rci_2016_vs_2013=X2016.vs.2013,
    rci_2013_vs_2010=X2013.vs.2010
  )

  unique_data <- test_uniqueness(
    rci_data, index_vars = c("iso3c", "NUTS.CODE.2016", "year"))

  if (unique_data){
    var_labels <- c(
      "iso3c"="Country code (iso3c)",
      "NUTS.CODE.2016"="NUTS V2 code",
      "merged.regions_code"="7 capital regions and their surrounding commuting belt are merged and given a new, fictitious region's code.",
      "capital.region"="Indicates whether region hosts country capital",
      "NUTS.NAME"="Name of the region",
      "year"="Year of observation",
      "rci_value2019"="Indicator z-score for 2019.",
      "rci_2019_vs_2016"="Comparison of z-scores in 2019 with 2016.",
      "rci_2016_vs_2013"="Comparison of z-scores in 2016 with 2013",
      "rci_2013_vs_2010"="Comparison of z-scores in 2013 with 2010"
    )

    Hmisc::label(rci_data) = as.list(
      var_labels[match(names(rci_data), names(var_labels))])
    rel_path <- "data-raw/rci-indicator-evolution.rds"
    saveRDS(rci_data, file = here::here(rel_path))
    print(paste0("Saved RCI data to: ", rel_path))
  } else {
    stop("Annual RCI data not unique. Please check manually!")
  }
  return(rci_data)
}
