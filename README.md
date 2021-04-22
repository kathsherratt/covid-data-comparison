# covid-data-comparison
 Compare sources of Covid-19 data for countries in Europe including
 - ECDC private data (held locally)
 - [ECDC public data](https://www.ecdc.europa.eu/en/publications-data/data-daily-new-cases-covid-19-eueea-country)
 - [JHU](https://github.com/CSSEGISandData/COVID-19)
 - [JRC](https://github.com/ec-jrc/COVID-19)
 
 Sequence for case/death data:
```
source("R/get-data.R")
source("R/summarise-data.R")
source("R/plot.R")
```

 Sequence for healthcare data:
```
source("R/get-data.R")
source("R/health-data.R")
```
