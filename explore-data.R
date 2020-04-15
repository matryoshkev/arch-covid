# Just a working script to explore data

# Dependencies
library("RCurl")
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")


# 
# Testing data 
# 

testing_data <- read_csv("data/data-testing.csv") %>% 
	# filter(state %in% c("MO", "IL"))
	# filter(state %in% c("NY", "NJ", "MA"))
	# filter(state %in% c("PA", "MI"))
	# filter(state %in% c("CA", "WA"))
	# filter(state %in% c("LA", "TX"))
	filter(state %in% c("GA", "FL"))
qplot(x = date, y = totalTestResults, color = state, data = testing_data)
qplot(x = date, y = totalTestResults, color = state, data = testing_data, log = "y")
qplot(x = date, y = positive, color = state, data = testing_data, log = "y")
qplot(x = date, y = negative, color = state, data = testing_data, log = "y")
qplot(
	x = date, y = positive/totalTestResults, color = state, 
	data = testing_data, ylim = c(0, 1)
)
# MO didn't start reporting good negative test numbers until 2020-03-28
# IL numbers stabilize at 2020-03-29


# 
# Case data 
# 

case_data <- read_csv("data/data-cases.csv")
case_data <- case_data %>%
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases")
case_data$date <- ymd(case_data$date)
qplot(
	x = date, y = cases, 
	data = subset(case_data, 
		(county == "St. Louis") & (state == "Missouri") & (date > ymd("2020-03-15"))
	), 
	log = "y"
)





