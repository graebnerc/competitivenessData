# TODO 1: fill out 'policyAreaInfo'
# TODO 2: Check the translation into implementation scores

#' Creates data on sub-assessments
#
#' Refers to confidential data of the Commission that is not made public
#' The function can be used to convert the file
#' CSRs_policy_area_conf (xlsx or csv)
#' to a usable RDS file. It is assumed that the original file lies in the
#' subfolder 'data-raw'. The final file will be saved in the subfolder 'data'.
#'
#' @param convert_csv T or F; indicates whether a csv version should be saved
#'  in 'data-raw'.
#' @return NULL; but saves the RDS file in the directory 'data'
create_CSR_sub_assessment <- function(convert_csv){
  orig_xlsx <- here::here("data-raw/CSRs_policy_area_conf.xlsx")
  csv_file <- here::here("data-raw/CSRs_policy_area_conf.csv")

  if (file.exists(orig_xlsx) | file.exists(csv_file)){
    if (convert_csv){
      orig_data <- readxl::read_xlsx(
        orig_xlsx, .name_repair = "universal")
      data.table::fwrite(orig_data, file = csv_file)
    }
    orig_data <-data.table::fread(
      file = here::here("data-raw/CSRs_policy_area_conf.csv"),
      select = c(
        "country"="character", "year"="double", "csrNumber"="character",
        "subpartNumber"="character", "policyAreas"="character",
        # "aa.maa"="character",
        # 'aa.maa': Only "AA" in 2019, and some observations with "" before;
        # but seems save to ignore since all other values are "MAA"
        "assessment"="character"))

    policy_areas <- unique(orig_data[["policyAreas"]])
    policy_areas_codes <- paste0("PA_", 1:length(policy_areas))
    policyAreaInfo <- tibble::tibble(
      PA_name = policy_areas,
      PA_code = policy_areas_codes,
      Competition = NA # TODO: add T/F whether it is relevant for competititon
    )
    data.table::fwrite(policyAreaInfo,
                       file = here::here("data-raw/policy_areas_blank.csv"))

    orig_data <- dplyr::mutate(
      orig_data,
      policyAreas=countrycode::countrycode(
        policyAreas, "PA_name", "PA_code", custom_dict = policyAreaInfo))

    orig_data <- dplyr::filter(orig_data, assessment!="")

    data.table::setDT(orig_data)

    orig_data[, n_csr:=max(csrNumber), .(country, year)]
    orig_data[, n_csr_sub:=.N, .(country, year)]
    orig_data[, n_csr_sub_total:=.N, .(year)]

    orig_data[, assessment_nb:=countrycode::countrycode(
      assessment, "assessment", "code") , .(country, year)]
    # TODO: Hier noch die scores checken
    assessment_score_list <- list( # Das wie ich dachte
      "Not Assessed"=NA,
      "No Progress"=0.0,
      "Limited Progress"=0.25,
      "Some Progress"=0.5,
      "Substantial Progress"=0.75,
      "Full Implementation"=1.0
    )
    assessment_score_list <- list( # Das wie es im Bruegel paper ist
      "Not Assessed"=NA,
      "No Progress"=0.0,
      "Limited Progress"=1.0,
      "Some Progress"=2.0,
      "Substantial Progress"=3.0,
      "Full Implementation"=4.0
    )

    orig_data[, assessment_score:=ifelse(
      assessment=="Not Assessed",
      assessment_score_list[["Not Assessed"]],
      ifelse(assessment=="No Progress",
             assessment_score_list[["No Progress"]],
             ifelse(assessment=="Limited Progress",
                    assessment_score_list[["Limited Progress"]],
                    ifelse(assessment=="Some Progress",
                           assessment_score_list[["Some Progress"]],
                           ifelse(assessment=="Substantial Progress",
                                  assessment_score_list[["Substantial Progress"]],
                                  ifelse(assessment=="Full Implementation",
                                         assessment_score_list[["Full Implementation"]],
                                         NA))))))]
    colsToDelete <- c("csrNumber", "subpartNumber", "assessment")
    orig_data[, (colsToDelete):=NULL]

    orig_data[, year_AAS:=mean(assessment_score, na.rm=T), .(year)]
    orig_data[, year_area_AAS:=mean(assessment_score, na.rm=T), .(year, policyAreas)]
    orig_data[, country_AAS:=mean(assessment_score, na.rm=T), .(country)]
    orig_data[, country_year_AAS:=mean(assessment_score, na.rm=T), .(year, country)]
    orig_data[, country_year_area_AAS:=mean(assessment_score, na.rm=T),
              .(year, country, policyAreas)]

    orig_data[, sharePA_country:=.N/n_csr_sub, .(year, country, policyAreas)]
    orig_data[, sharePA_all:=.N/n_csr_sub_total, .(year, policyAreas)]
    orig_data[, country:=countrycode::countrycode(country, "eurostat", "iso3c")]

    names_labels <- c(
      "country"="The iso3c country code",
      "year"="The year of assessment",
      "policyAreas"="The policy area",
      "n_csr"="Total nb of CSR for this country in this year",
      "n_csr_sub"="Total number of sub-CSR for this country for this year.",
      "n_csr_sub_total"="Total number of sub-CSR for all countries in this year",
      "assessment_score"="The numerical assessment score",
      "year_AAS"="The average assessment score for this year (taking into account all countries).",
      "year_area_AAS"="The average assessment score for this area in this year (taking into account all countries).",
      "country_AAS"="The average assessment score for this country over all years.",
      "country_year_AAS"="The average assessment score for this country in this year.",
      "country_year_area_AAS"="The average assessment score for this country in this policy area in this year.",
      "sharePA_country"="The share of recommendations in the area of all recommendations in this year for this country.",
      "sharePA_all"="The share of recommendations in the area of all recommendations in this year."
    )

    Hmisc::label(orig_data) = as.list(
      names_labels[match(names(orig_data), names(names_labels))])

    final_file <- here::here("data/CSR_policy_areas.rds")
    saveRDS(orig_data, file = final_file)
    print(paste0("Saved confidential data to: ", final_file))
  } else{
    warning(paste0("The input file is missing. Note that this file ",
                   "containst confidential information and is not shipped ",
                   "alongside the package. It is assumed that the original ",
                   "file lies in the sub-directory 'data' and is called: ",
                   orig_xlsx))
  }
  return(NULL)
}

create_CSR_sub_assessment(
  convert_csv = F # Convert the original xlsx file to csv?
)
