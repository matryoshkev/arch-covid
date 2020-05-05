# Dependencies: 
library("RCurl")
library("readr")
library("dplyr")
library("lubridate")


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
	"New Orleans" = c(
		22051,  # Jefferson Parish LA
		22071,  # Orleans Parish LA
		22103   # St. Tammany Parish LA
		# 22089  # St. Charles Parish LA
		# 22087  # St. Bernard Parish LA
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
		FIPS %in% metro_area[["New Orleans"]] ~ "New Orleans", 
		FIPS %in% metro_area[["St. Louis"]]   ~ "St. Louis" 
	))
write_csv(data_cases, "data/data-cases.csv")


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
write_csv(data_testing, "data/data-testing.csv")


# 
# Clean up
# 

rm(metro_area, data_cases, data_testing)

