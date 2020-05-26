# TO DO: 
# - clean up for new statistical areas

# Dependencies: 
library("RCurl")
library("readr")
library("dplyr")
library("lubridate")
library("stringr")
library("tidycensus")

# # Define US metropolitan areas by county FIPS code (Federal Information Processing Standard)
# metro_area <- list(
	# "Atlanta" = c(
		# 13121,  # Fulton County GA
		# 13135,  # Gwinnet County GA
		# 13067,  # Cobb County GA
		# 13089,  # DeKalb County GA
		# 13063,  # Clayton County GA
		# 13057,  # Cherokee County GA
		# 13117,  # Forsyth County GA
		# 13151   # Henry County GA
		# # And many more, depending on how you count
	# ), 
	# "Boston" = c(
		# 25021,  # Norfolk County MA
		# 25023,  # Plymouth County MA
		# 25025,  # Suffolk County MA
		# 25009,  # Essex County MA
		# 25017,  # Middlesex County MA
		# 33015,  # Rockingham County NH
		# 33017   # Strafford County NH
	# ), 
	# "Chicago" = c(
		# 17031,  # Cook County IL
		# 17043,  # DuPage County IL
		# 17197,  # Will County IL
		# 17089,  # Kane County IL
		# 17111,  # McHenry County IL
		# 17091,  # Kankakee County IL
		# # 17037,  # DeKalb County IL
		# 17093   # Kendall County IL
		# # 17063	# Grundy County IL
	# ), 
	# "Dallas-Fort Worth" = c(
		# 48113,  # Dallas County TX
		# 48439,  # Tarrant County TX
		# 48085,  # Collin County TX
		# 48121  # Denton County TX
		# # 48139,  # Ellis County TX
		# # 48251,  # Johnson County TX
		# # 48367,  # Parker County TX
		# # 48257  # Kaufman County TX
		# # and several smaller...
	# ), 
	# "Detroit" = c(
		# 26087,  # Lapeer County MI
		# 26093,  # Livingston County MI
		# 26099,  # Macomb County MI
		# 26125,  # Oakland County MI
		# 26147,  # St. Clair County MI
		# 26163   # Wayne County MI
	# ), 
	# "Houston" = c(
		# 48201,  # Harris County TX
		# 48157,  # Fort Bend County TX
		# 48339,  # Montgomery County TX
		# 48039,  # Brazoria County TX
		# 48167   # Galveston County TX
		# # 48291   # Liberty County TX
		# # 48437   # Waller County TX
		# # 48071   # Chambers County TX
		# # 48015   # Austin County TX
	# ), 
	# "Los Angeles" = c(
		# 6037,  # Los Angeles County CA
		# 6059   # Orange County CA
	# ), 
	# "Miami" = c(
		# 12086,  # Miami-Dade County FL
		# 12011,  # Broward County FL
		# 12099   # Palm Beach County FL
	# ), 
	# "New Orleans" = c(
		# 22051,  # Jefferson Parish LA
		# 22071,  # Orleans Parish LA
		# 22103   # St. Tammany Parish LA
		# # 22089  # St. Charles Parish LA
		# # 22087  # St. Bernard Parish LA
	# ), 
	# "New York" = c(
		# 36047,  # Kings County NY (Brooklyn)
		# 36081,  # Queens County NY (Queens)
		# 36061,  # New York County NY (Manhattan)
		# 36005,  # Bronx County NY (Bronx)
		# 36085  # Richmond County NY (Staten Island)
	# ), 
	# "Seattle" = c(
		# 53033,  # King County WA
		# 53061,  # Snohomish County WA
		# 53053   # Pierce County WA
	# ), 
	# "St. Louis" = c(
		# 29510,  # St. Louis City MO
		# 29189,  # St. Louis County MO
		# 29183,  # St. Charles County MO
		# 17119,  # Madison County IL
		# 17163,  # St. Clair County IL
		# 29099   # Jefferson County MO
		# # 29071   # Franklin County MO
		# # 29113,  # Lincoln County MO
		# # 29510,  # Warren County MO
		# # 17133,  # Monroe County IL
		# # 17083,  # Jersey County IL
		# # 17027   # Clinton County IL
		# #   # Bond County
		# #   # Calhoun County
		# #   # Macoupin County
	# )
# )


# 
# Load US counties and statistical areas
# 

data_counties <- 
	tibble(tidycensus::fips_codes) %>%
	rename(state_abbrev = state, state = state_name) %>%
	mutate(FIPS = str_c(state_code, county_code)) %>%
	select(FIPS, state_code, county_code, state, state_abbrev, county)
	# rename(
		# state_abbrev = state, 
		# # FIPS_state_code  = state_code, 
		# # FIPS_county_code = county_code, 
		# # county_name      = county
	# ) %>%
	# mutate(FIPS = str_c(FIPS_state_code, FIPS_county_code)) %>%
	# select(FIPS, FIPS_state_code, FIPS_county_code, state_name, state_abbrev, county_name)
data_areas    <- read_csv("data/statistical-areas.csv", col_types = paste(rep("c", 13), collapse = ""))
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

# data_cases <- left_join(data_cases, data_population, by = "FIPS") %>%
	# select(FIPS, county, state, population, everything())



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

	# select(-c(UID, iso2, iso3, code3, Combined_Key, Country_Region, Lat, Long_)) %>%
	# rename(
		# county = Admin2, 
		# state  = Province_State
	# ) %>% 
	# mutate(metro = case_when(
		# FIPS %in% metro_area[["Atlanta"]]     ~ "Atlanta", 
		# FIPS %in% metro_area[["Chicago"]]     ~ "Chicago", 
		# FIPS %in% metro_area[["Dallas-Fort Worth"]] ~ "Dallas-Fort Worth", 
		# FIPS %in% metro_area[["Detroit"]]     ~ "Detroit", 
		# FIPS %in% metro_area[["Houston"]]     ~ "Houston", 
		# FIPS %in% metro_area[["Los Angeles"]] ~ "Los Angeles", 
		# FIPS %in% metro_area[["Boston"]] ~ "Boston", 
		# FIPS %in% metro_area[["Miami"]]       ~ "Miami", 
		# FIPS %in% metro_area[["New Hampshire"]] ~ "New Hampshire", 
		# FIPS %in% metro_area[["New Orleans"]] ~ "New Orleans", 
		# FIPS %in% metro_area[["New York"]]    ~ "New York", 
		# FIPS %in% metro_area[["Seattle"]]     ~ "Seattle", 
		# FIPS %in% metro_area[["St. Louis"]]   ~ "St. Louis" 
	# ))



# Get case testing data 
# from The COVID Tracking Project (a project of The Atlantic)
# https://covidtracking.com

data_testing <- RCurl::getURL("https://covidtracking.com/api/v1/states/daily.csv")
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
rm(metro_area, data_cases, data_testing, data_population)

