# Dependencies: 
# library("RCurl")
# library("readr")
# library("dplyr")
# library("lubridate")


# Get case testing data 
# from The COVID Tracking Project (a project of The Atlantic)
# https://covidtracking.com

testing_data <- RCurl::getURL("https://covidtracking.com/api/v1/states/daily.csv")
testing_data <- readr::read_csv(testing_data)  
testing_data <- testing_data %>% 
	select(date, state, positive, negative, totalTestResults) %>%
	mutate(date = ymd(date))  
write_csv(testing_data, "data/data-testing.csv")
	# Comes in long format


# Get confirmed case data 
# from Johns Hopkins Center for Systems Science and Engineering (CSSE)
# https://github.com/CSSEGISandData/COVID-19
# 
# Unfortunately ambiguous in files and on GitHub, but 
# data are counts of **cumulative** confirmed cases as of date listed

case_data <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
case_data <- readr::read_csv(case_data)
case_data <- case_data %>%
	rename_at(vars(contains("/")), list(mdy)) %>%   # format dates as YYYY-MM-DD
	select(-c(iso2, iso3, code3, FIPS, Country_Region, Lat, Long_)) %>%
	rename(
		county       = Admin2, 
		state        = Province_State, 
		county_label = Combined_Key
	)
write_csv(case_data, "data/data-cases.csv")
	# Comes in wide format


# 
# Clean up
# 

rm(testing_data, case_data)

