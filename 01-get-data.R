
# Dependencies: 
library("RCurl")
library("readr")
library("dplyr")
library("lubridate")
library("tidycensus")

# Define US metropolitan areas by county FIPS code (Federal Information Processing Standard)
metro_area <- list(
	"Atlanta" = c(
		13121,  # Fulton County GA
		13135,  # Gwinnet County GA
		13067,  # Cobb County GA
		13089,  # DeKalb County GA
		13063,  # Clayton County GA
		13057,  # Cherokee County GA
		13117,  # Forsyth County GA
		13151   # Henry County GA
		# And many more, depending on how you count
	), 
	"Chicago" = c(
		17031,  # Cook County IL
		17043,  # DuPage County IL
		17197,  # Will County IL
		17089,  # Kane County IL
		17111,  # McHenry County IL
		17091,  # Kankakee County IL
		# 17037,  # DeKalb County IL
		17093   # Kendall County IL
		# 17063	# Grundy County IL
	), 
	"Detroit" = c(
		26087,  # Lapeer County MI
		26093,  # Livingston County MI
		26099,  # Macomb County MI
		26125,  # Oakland County MI
		26147,  # St. Clair County MI
		26163   # Wayne County MI
	), 
	"Houston" = c(
		48201,  # Harris County TX
		48157,  # Fort Bend County TX
		48339,  # Montgomery County TX
		48039,  # Brazoria County TX
		48167   # Galveston County TX
		# 48291   # Liberty County TX
		# 48437   # Waller County TX
		# 48071   # Chambers County TX
		# 48015   # Austin County TX
	), 
	"Los Angeles" = c(
		6037  # Los Angeles County CA
	), 
	"Miami" = c(
		12086,  # Miami-Dade County FL
		12011,  # Broward County FL
		12099   # Palm Beach County FL
	), 
	"New Orleans" = c(
		22051,  # Jefferson Parish LA
		22071,  # Orleans Parish LA
		22103   # St. Tammany Parish LA
		# 22089  # St. Charles Parish LA
		# 22087  # St. Bernard Parish LA
	), 
	"New York" = c(
		36047,  # Kings County NY (Brooklyn)
		36081,  # Queens County NY (Queens)
		36061,  # New York County NY (Manhattan)
		36005,  # Bronx County NY (Bronx)
		36085  # Richmond County NY (Staten Island)
	), 
	"Seattle" = c(
		53033,  # King County WA
		53061,  # Snohomish County WA
		53053   # Pierce County WA
	), 
	"St. Louis" = c(
		29510,  # St. Louis City MO
		29189,  # St. Louis County MO
		29183,  # St. Charles County MO
		17119,  # Madison County IL
		17163,  # St. Clair County IL
		29099   # Jefferson County MO
		# 29071   # Franklin County MO
		# 29113,  # Lincoln County MO
		# 29510,  # Warren County MO
		# 17133,  # Monroe County IL
		# 17083,  # Jersey County IL
		# 17027   # Clinton County IL
		#   # Bond County
		#   # Calhoun County
		#   # Macoupin County
	)
)


# Get confirmed case data 
# from Johns Hopkins Center for Systems Science and Engineering (CSSE)
# https://github.com/CSSEGISandData/COVID-19
# 
# Data are counts of **cumulative** confirmed cases as of date listed

data_cases <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
data_cases <- readr::read_csv(data_cases)
write_csv(data_cases, "data/data-cases-raw.csv")
data_cases <- data_cases %>%
	rename_at(vars(contains("/")), list(mdy)) %>%   # format dates as YYYY-MM-DD
	select(-c(UID, iso2, iso3, code3, Combined_Key, Country_Region, Lat, Long_)) %>%
	rename(
		county = Admin2, 
		state  = Province_State
	) %>% 
	mutate(metro = case_when(
		FIPS %in% metro_area[["Atlanta"]]     ~ "Atlanta", 
		FIPS %in% metro_area[["Chicago"]]     ~ "Chicago", 
		FIPS %in% metro_area[["Detroit"]]     ~ "Detroit", 
		FIPS %in% metro_area[["Houston"]]     ~ "Houston", 
		FIPS %in% metro_area[["Los Angeles"]] ~ "Los Angeles", 
		FIPS %in% metro_area[["Miami"]]       ~ "Miami", 
		FIPS %in% metro_area[["New Orleans"]] ~ "New Orleans", 
		FIPS %in% metro_area[["New York"]]    ~ "New York", 
		FIPS %in% metro_area[["Seattle"]]     ~ "Seattle", 
		FIPS %in% metro_area[["St. Louis"]]   ~ "St. Louis" 
	))


# Get case testing data 
# from The COVID Tracking Project (a project of The Atlantic)
# https://covidtracking.com

data_testing <- RCurl::getURL("https://covidtracking.com/api/v1/states/daily.csv")
data_testing <- readr::read_csv(data_testing)  
write_csv(data_testing, "data/data-testing-raw.csv")
data_testing <- data_testing %>% 
	select(date, state, positive, negative, totalTestResults) %>%
	rename(total = totalTestResults) %>%
	mutate(date = ymd(date))  


# Get population data
# from 2018 American Community Survey

# B01003_001E  # Estimate!!Total	TOTAL POPULATION

# get_acs(geography = "county", variables = "B01003_001E", state = "MO")
# get_estimates(geography = "metropolitan statistical area/micropolitan statistical area", product = "population")
# get_estimates(geography = "state", product = "population", state = "MO")
# head(fips_codes)
# subset(fips_codes, state == "MO")

data_population <- get_acs(geography = "county", variables = "B01003_001E")
data_population <- data_population %>%
	rename(FIPS = GEOID, population = estimate) %>%
	mutate(FIPS = as.numeric(FIPS)) %>%
	select(FIPS, population)

data_cases <- left_join(data_cases, data_population, by = "FIPS") %>%
	select(FIPS, county, state, population, everything())


# Write data to files
write_csv(data_cases, "data/data-cases.csv")
write_csv(data_testing, "data/data-testing.csv")

# Clean up
rm(metro_area, data_cases, data_testing, data_population)

