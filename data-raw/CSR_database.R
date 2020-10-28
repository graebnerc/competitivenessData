#' Setup the implementation scores from the comission
#'
#' Takes the csv file that was created from the excel file obtained from the
#'  EU webpage and provides a data.table with the mean implementation scores
#'  and means for the MIP, SGP, INTEGRATED and SGP/MIT related country-specific
#'  recommendations.
setup_imp_scores <- function(){
  warning(
    paste0(
      "The implementation scores were obtained as an excel file from the webpage mentioned in the README and the script. Download cannot be automated by must be made manually. Current version: 75; the csv file used is identical to the original excel except the header."
    )
  )

  imp_scores <- data.table::fread(
    file = here::here("data-raw/CSR_database_v78_final.csv"),
    sep = ";", dec = ",", colClasses = "character", header = T)

  cols2keep <- c(
    "Member States", "Year",
    paste0("Legal Basis: Art. 121/148 (integrated guidelines)",
           " + secondary legislation (SGP or MIP)"),
    "Assessment Score")

  imp_scores <- imp_scores[, ..cols2keep]
  data.table::setnames(
    imp_scores,
    old = c("Year",
            paste0("Legal Basis: Art. 121/148 (integrated guidelines)",
                   " + secondary legislation (SGP or MIP)"),
            "Member States", "Assessment Score"),
    new = c("year", "LegalBasis", "iso3c", "AIS"))
  imp_scores[, iso3c:=countrycode::countrycode(iso3c, "eurostat", "iso3c")]
  imp_scores[, AIS:=gsub(",", ".", AIS)]
  imp_scores[, LegalBasis:=gsub("/", "_", LegalBasis)]
  imp_scores[, AIS:=as.double(ifelse((AIS%in%c("", "None")), NA, AIS))]
  imp_scores <- data.table::copy(imp_scores)
  imp_scores[, AIS_mean:=mean(AIS, na.rm = T), .(iso3c, year)]
  imp_scores[, AIS_mean_sub:=mean(AIS, na.rm = T), .(iso3c, year, LegalBasis)]
  imp_scores[, AIS:=NULL]
  imp_scores <- unique(imp_scores)
  imp_scores <- tidyr::pivot_wider(
    imp_scores, names_from="LegalBasis",
    values_from="AIS_mean_sub")
  data.table::setDT(imp_scores)
  imp_scores[, year:=as.double(year)]
  data.table::setnames(
    imp_scores,
    old = names(imp_scores)[4:7],
    new = paste0("AIS_mean_", names(imp_scores)[4:7]))
  data.table::setnafill(
    x = imp_scores, type = "const", fill = NA, nan = NA,
    cols = c("AIS_mean", "AIS_mean_MIP", "AIS_mean_SGP",
             "AIS_mean_Integrated", "AIS_mean_SGP_MIP"))
  name_labels <- c(
    "iso3c"= NULL,# "iso3c",
    "year"= NULL, #"time",
    "AIS_mean"="The mean implementation score for the country in this year",
    "AIS_mean_MIP"="Same as AIS_mean, but only for scores with MIP as the legal basis",
    "AIS_mean_SGP"="Same as AIS_mean, but only for scores with SGP as the legal basis",
    "AIS_mean_Integrated"="Same as AIS_mean, but only for scores with Integrated as the legal basis",
    "AIS_mean_SGP_MIP"="Same as AIS_mean, but only for scores with SGP/MIP as the legal basis"
  )
  Hmisc::label(imp_scores) = as.list(
    name_labels[match(names(imp_scores), names(name_labels))])
  test_uniqueness(imp_scores, c("iso3c", "year"))
  return(imp_scores)
}
