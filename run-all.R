# Epidemiological population dynamics of SARS-CoV-2/COVID-19
# jeff smith 
# 2020-04

# Dependencies
library("RCurl")
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("forcats")
library("ggplot2")
library("scales")
library("gtable")

# Scripts
source("01-get-data.R")
source("02-plot-total.R")
source("03-analyze-testing.R")
source("04-plot-activity.R")


