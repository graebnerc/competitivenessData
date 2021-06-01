#' Get data from World Bank Database
#'
#' Downloads indicators from World Bank database.
#'
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
get_worldbank <- function(download_data, countries_considered,
                          first_year, last_year){
  print("World Bank data...")
  # TODO Add export_GDP
  # TODO Download all countries and filter those we do not need
  #
  wb_vars <- c(
    "BX.KLT.DINV.WD.GD.ZS", # Foreign direct investment, net inflows (% of GDP): https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
    "BM.KLT.DINV.WD.GD.ZS", # Foreign direct investment, net outflows (% of GDP): https://data.worldbank.org/indicator/BM.KLT.DINV.WD.GD.ZS
    "GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP): https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
    "GC.TAX.INTT.RV.ZS", # Taxes on international trade (% of revenue): https://data.worldbank.org/indicator/GC.TAX.INTT.RV.ZS
    "GC.TAX.YPKG.RV.ZS", # Taxes on income, profits and capital gains (% of revenue): https://data.worldbank.org/indicator/GC.TAX.YPKG.RV.ZS
    "NE.TRD.GNFS.ZS", # Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product: https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
    "NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS
    "NE.IMP.GNFS.ZS", # Imports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS
    "GB.XPD.RSDV.GD.ZS", # Research and development expenditure (% of GDP): https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
    "SP.POP.SCIE.RD.P6", # The number of researchers engaged in Research &Development (R&D), expressed as per million: SP.POP.SCIE.RD.P6
    "VA.EST", # WGI: Voice and Accountability; WGI home: https://info.worldbank.org/governance/wgi/#home API: https://api.worldbank.org/v2/sources/3/indicators
    "RQ.EST", # WGI: Regulatory Quality
    "RL.EST", # WGI: Rule of Law
    "PV.EST", # WGI: Political Stability and Absence of Violence/Terrorism
    "GE.EST", # WGI: Government Effectiveness
    "CC.EST", # WGI: Control of Corruption
    "sl.ind.empl.zs", # Employment in industry (% of total employment): https://data.worldbank.org/indicator/sl.ind.empl.zs
    "SL.AGR.EMPL.ZS", # Employment in agriculture (% of total employment): https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS
    "SL.SRV.EMPL.ZS", # Employment in services (% of total employment): https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS
    "SL.EMP.SELF.ZS", # Self-employed (% of total employment): https://data.worldbank.org/indicator/SL.EMP.SELF.ZS
    "SL.UEM.NEET.ZS", # Share of youth not in education, employment of training (% of youth population): https://data.worldbank.org/indicator/SL.UEM.NEET.ZS
    "NV.IND.TOTL.ZS", # Industry (including construction), value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.TOTL.ZS
    "NV.IND.MANF.ZS", # Manufacturing, value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.MANF.ZS
    "BN.CAB.XOKA.GD.ZS", # Current account balance (% of GDP): https://data.worldbank.org/indicator/BN.CAB.XOKA.GD.ZS
    "SP.POP.TOTL", # Population: https://data.worldbank.org/indicator/SP.POP.TOTL
    "ny.gdp.totl.rt.zs", # Natural resource rents: https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
    "NY.GDP.MKTP.KN", # Real GDP (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KN
    "NY.GDP.MKTP.KD.ZG", # Real GDP growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
    "NY.GDP.PCAP.KN", # Real GDP per capita (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KN
    "NY.GDP.PCAP.KD.ZG", # Real GDP per capita growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG
    "NY.GDP.MKTP.KD", # Real GDP (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
    "NY.GDP.PCAP.KD", # Real GDP per capita (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
    "NY.GDP.MKTP.CN", # Nominal GDP (current LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.CN
    "NY.GDP.PCAP.CN", # Nominal GDP per capita (current LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
    "NY.GDP.MKTP.CD", # Nominal GDP (current US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
    "NY.GDP.PCAP.CD", # Nominal GDP per capita (current US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
    "NY.GDP.MKTP.PP.KD", # GDP, PPP (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
    "NY.GDP.PCAP.PP.KD", # GDP, PPP per capita (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
    "NY.GDP.MKTP.PP.CD", # GDP, PPP (current int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
    "NY.GDP.PCAP.PP.CD", # GDP, PPP per capita (current int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
    "SL.UEM.TOTL.ZS" # Unemployment rate from World Bank: https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
  )

  wb_var_names <- c(
    "fdi_net_inflow_GDP", # Foreign direct investment, net inflows (% of GDP): https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
    "fdi_net_outflow_GDP", # Foreign direct investment, net outflows (% of GDP): https://data.worldbank.org/indicator/BM.KLT.DINV.WD.GD.ZS
    "tax_rev_total_GDP", # Tax revenue (% of GDP): https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
    "tax_rev_trade_TAX", # Taxes on international trade (% of revenue): https://data.worldbank.org/indicator/GC.TAX.INTT.RV.ZS
    "tax_rev_inc_profits_TAX", # Taxes on income, profits and capital gains (% of revenue): https://data.worldbank.org/indicator/GC.TAX.YPKG.RV.ZS
    "trade_total_GDP", # Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product: https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
    "trade_exp_GDP", # Exports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS
    "trade_imp_GDP", # Imports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS
    "RD_expend_GDP", # Research and development expenditure (% of GDP): https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
    "RD_scientists", # The number of researchers engaged in Research &Development (R&D), expressed as per million: SP.POP.SCIE.RD.P6
    "wgi_accountability", # WGI: Voice and Accountability; WGI home: https://info.worldbank.org/governance/wgi/#home API: https://api.worldbank.org/v2/sources/3/indicators
    "wgi_regul_quality", # WGI: Regulatory Quality
    "wgi_rule_of_law", # WGI: Rule of Law
    "wgi_pol_stability", # WGI: Political Stability and Absence of Violence/Terrorism
    "wgi_gov_effectvn", # WGI: Government Effectiveness
    "wgi_control_corrupt", # WGI: Control of Corruption
    "empl_ind", # Employment in industry (% of total employment): https://data.worldbank.org/indicator/sl.ind.empl.zs
    "empl_agr", # Employment in agriculture (% of total employment): https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS
    "empl_serv", # Employment in services (% of total employment): https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS
    "empl_self", # Self-employed (% of total employment): https://data.worldbank.org/indicator/SL.EMP.SELF.ZS
    "unemp_youth_neet", # Share of youth not in education, employment of training (% of youth population): https://data.worldbank.org/indicator/SL.UEM.NEET.ZS
    "VA_industry_gdp", # Industry (including construction), value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.TOTL.ZS
    "VA_manufct_gdp", # Manufacturing, value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.MANF.ZS
    "current_account_GDP_WB", # Current account balance (% of GDP): https://data.worldbank.org/indicator/BN.CAB.XOKA.GD.ZS
    "population", # Population: https://data.worldbank.org/indicator/SP.POP.TOTL
    "res_rents", # Natural resource rents: https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
    "gdp_real_lcu", # Real GDP (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KN
    "gdp_real_lcu_growth", # Real GDP growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
    "gdp_real_pc_lcu", # Real GDP per capita (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KN
    "gdp_real_pc_lcu_growth", # Real GDP per capita growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG
    "gdp_real_usd", # Real GDP (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
    "gdp_real_pc_usd", # Real GDP per capita (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
    "gdp_nom_lcu", # Nominal GDP (current LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.CN
    "gdp_nom_pc_lcu", # Nominal GDP per capita (current LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
    "gdp_nom_usd", # Nominal GDP (current US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
    "gdp_nom_pc_usd", # Nominal GDP per capita (current US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
    "gdp_real_ppp", # GDP, PPP (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
    "gdp_real_pc_ppp", # GDP, PPP per capita (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
    "gdp_nom_ppp", # GDP, PPP (current int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
    "gdp_nom_pc_ppp", # GDP, PPP per capita (current int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
    "unemp_rate_wb" # Unemployment rate from World Bank: https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
  )

  wb_vars_2 <- c(
    "sl.tlf.totl.in", # Total labor force:  https://data.worldbank.org/indicator/SL.TLF.TOTL.IN
    "FP.CPI.TOTL", # https://data.worldbank.org/indicator/FP.CPI.TOTL
    "FP.CPI.TOTL.ZG", # Inflation: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
    "FR.INR.RINR", # https://data.worldbank.org/indicator/FR.INR.RINR
    "NE.GDI.FTOT.ZS", # Gross fixed capital formation (% of GDP)
    "NE.GDI.FTOT.KN", # Gross fixed capital formation (constant LCU)
    "NE.GDI.FTOT.KD" # Gross fixed capital formation (constant 2010 US$)
  )

  wb_var_names_2 <- c(
    "labor_force_total", # Total labor force:  https://data.worldbank.org/indicator/SL.TLF.TOTL.IN
    "cpi_wb", # https://data.worldbank.org/indicator/FP.CPI.TOTL
    "cpi_change_wb", # Inflation: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
    "interest_real", # https://data.worldbank.org/indicator/FR.INR.RINR
    "cap_form_gf_perc_gdp", # Gross fixed capital formation (% of GDP)
    "cap_form_gf_real_lcu", # Gross fixed capital formation (constant LCU)
    "cap_form_gf_real_usd" # Gross fixed capital formation (constant 2010 US$)
  )

  wb_file_name <- "data-raw/wb_data.csv"
  if (download_data | !file.exists(paste0(wb_file_name, ".gz"))){
    if (!download_data){
      warning("File for World Bank data does not exist. Download from www...")
    }
    wb_raw_data <- data.table::as.data.table(
      WDI::WDI(country = countrycode::countrycode(countries_considered,
                                                  "iso3c", "iso2c"),
               indicator = wb_vars,
               start = first_year, end = last_year)
    )
    wb_raw_data_2 <- data.table::as.data.table(
      WDI::WDI(country = countries_considered,
               indicator = wb_vars_2,
               start = first_year, end = last_year)
    )

    wb_raw_data <- wb_raw_data[wb_raw_data_2, on=c("iso2c", "country", "year")]

    data.table::fwrite(wb_raw_data, wb_file_name)
    R.utils::gzip(paste0(wb_file_name),
                  destname=paste0(wb_file_name, ".gz"),
                  overwrite = TRUE, remove = TRUE)
  } else {
    wb_raw_data <- data.table::fread(paste0(wb_file_name, ".gz"))
  }

  data.table::setnames(wb_raw_data, old = c(wb_vars, wb_vars_2),
                       new = c(wb_var_names, wb_var_names_2))
  wb_data <- wb_raw_data[, iso3c:=countrycode::countrycode(iso2c,
                                                           "iso2c", "iso3c")
  ][, c("iso2c", "country"):=NULL]
  wb_data[, year:=as.integer(year)]
  stopifnot(test_uniqueness(wb_data, c("year", "iso3c")))
  print("finished.")
  return(wb_data)
}

