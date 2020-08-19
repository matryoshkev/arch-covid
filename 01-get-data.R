# Dependencies: 
library("RCurl")
library("readr")
library("dplyr")
library("lubridate")
library("stringr")
library("tidycensus")

# 
# Load US counties and statistical areas
# 

data_counties <- 
	tibble(tidycensus::fips_codes) %>%
	rename(state_abbrev = state, state = state_name) %>%
	mutate(FIPS = str_c(state_code, county_code)) %>%
	select(FIPS, state_code, county_code, state, state_abbrev, county)
data_areas <- read_csv("data/statistical-areas.csv", col_types = paste(rep("c", 13), collapse = ""))
data_counties <- left_join(data_counties, data_areas)

# Check for duplicates created by mismatches: 
# nrow(data_counties)
# length(unique(data_counties$FIPS))

# Get population data from 2018 American Community Survey
# B01003_001E  # Estimate!!Total	TOTAL POPULATION
data_population <- 
	tidycensus::get_acs(geography = "county", variables = "B01003_001E") %>%
	rename(FIPS = GEOID, population = estimate) %>%
	select(FIPS, population)
data_counties <- left_join(data_counties, data_population, by = "FIPS")


# Get confirmed case data 
# from Johns Hopkins Center for Systems Science and Engineering (CSSE)
# https://github.com/CSSEGISandData/COVID-19
# 
# Data are counts of **cumulative** confirmed cases as of date listed

data_cases <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
data_cases <- readr::read_csv(data_cases)
write_csv(data_cases, "data/data-cases-raw.csv")

data_cases <- read_csv("data/data-cases-raw.csv") %>%
	filter(code3 == 840) %>%  # Exclude territories
	mutate(
		# Extract proper 5-digit FIPS string from UID
		UID = as.character(UID), FIPS = str_sub(UID, 4) 
	) %>%  
	select(FIPS, contains("/")) %>%
	rename_at(vars(contains("/")), list(mdy))  	# format dates as YYYY-MM-DD
data_cases <- inner_join(data_counties, data_cases, by = "FIPS")


# Get case testing data 
# from The COVID Tracking Project (a project of The Atlantic)
# https://covidtracking.com

# data_testing <- RCurl::getURL("https://covidtracking.com/api/v1/states/daily.csv")
data_testing <- RCurl::getURL("https://api.covidtracking.com/v1/states/daily.csv")
data_testing <- readr::read_csv(data_testing)  
write_csv(data_testing, "data/data-testing-raw.csv")
data_testing <- data_testing %>% 
	select(date, state, positive, negative, totalTestResults) %>%
	rename(total = totalTestResults, state_abbrev = state) %>%
	mutate(date = ymd(date))  

# state_names <- fips_codes %>%
	# select(state, state_name) %>%
	# distinct()

data_states  <- data_counties %>% select(state_code, state_abbrev, state) %>% distinct()
data_testing <- inner_join(data_testing, data_states)


# Write data to files
write_csv(data_cases, "data/data-cases.csv")
write_csv(data_testing, "data/data-testing.csv")

# Clean up
# rm(metro_area, data_cases, data_testing, data_population)

