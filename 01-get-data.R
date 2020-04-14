
# Dependencies
library("RCurl")
library("readr")
library("dplyr")
library("lubridate")


# Get case testing data from The Covid Tracking Project 
# (https://covidtracking.com, a project of The Atlantic)

testing_url  <- paste0("https://covidtracking.com/api/v1/states/daily.csv")
testing_data <- RCurl::getURL(testing_url)
testing_data <- readr::read_csv(testing_data)
testing_data <- testing_data %>% 
	select(date, state, positive, negative, totalTestResults) %>%
	filter(state %in% c("MO", "IL")) %>%
	mutate(date = ymd(date))
write_csv(testing_data, "data/data-testing.csv")

# Quick look: 
library("ggplot2")
qplot(x = date, y = totalTestResults, color = state, data = testing_data)
qplot(x = date, y = totalTestResults, color = state, data = testing_data, log = "y")
qplot(x = date, y = positive/totalTestResults, color = state, data = testing_data)

