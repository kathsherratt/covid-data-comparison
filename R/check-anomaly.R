# Anomaly checking

# Get data from all sources
source(here("get-euro-data.R"))
data <- get_euro_data()

# Check anomalies in JHU
source("https://raw.githubusercontent.com/reichlab/covidData/master/R/identify_outliers.R")

# Check differences between data sources




