# EU competitiveness data

The package provides data on competition and competitiveness on the EU level.
The data can either be downloaded directly from the directory `data`, or,
preferable if you use `R`, be accessed by installing the package:

```
devtools::install_github("graebnerc/competitivenessData")
```

The you can access the data either via:

````
competitivenessData::competitiveness_data_macro
competitivenessData::rci_data_annual
```

Or you can load it via:

```
data("competitiveness_data_macro", package = "competitivenessData")
data("competitiveness_data_macro", package = "competitivenessData")
```

Make sure to update the package to get the most recent version.

## Variable description

For the moment, at least the `*.rds` files include the labels of the variables,
which explain the variables in more detail. 
Labels were created via `Hmisc::label()` and can be retrieved either via
`View()` or via `Hmisc::label(NAME_OF_DATAFRAME)`.

## Variables considered so far

### Annual and national competitiveness data

Data can be found in the folder `data/competitiveness_data_annual`.

* The indicators for the [Macroeconomic Imbalance Scoreboard](https://ec.europa.eu/info/business-economy-euro/economic-and-fiscal-policy-coordination/eu-economic-governance-monitoring-prevention-correction/macroeconomic-imbalance-procedure_de)
* The indicators for the [Price and competitiveness report](https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/price-and-cost-competitiveness_en),
but so far only the annual data has been considered; weight matrices, monthly 
and quarterly data have not been considered so far
* Three indicators providing information about the status of a country in the MIP and the SGP surveillance (constructed by the authors)

### Data on the regional level

The EU has developed a *Regional Competitiveness Indicator*, which measures 
competitivenss in various dimensions on the NUTS-2 level.
It is built on 70 indicators.
The raw data is available for all, but a bit tricky to compile.
There are four editions:

* [The 2010 edition](https://ec.europa.eu/regional_policy/en/information/publications/working-papers/2011/a-new-regional-competitiveness-index-theory-methods-and-findings)
* [The 2013 edition](https://ec.europa.eu/regional_policy/en/information/publications/studies/2013/eu-regional-competitiveness-index-rci-2013)
* [The 2016 edition](https://ec.europa.eu/regional_policy/en/information/maps/regional_competitiveness/2016/)
* [The 2019 edition](https://ec.europa.eu/regional_policy/en/information/maps/regional_competitiveness/)

Currently, only the aggregated data is contained here, but it might be helpful
to have a look at the methodological annex of the editions, particularly the
latest one. We might be interested in some of the raw data as well.

The RCI data is in `data/rci_data_annual`.
