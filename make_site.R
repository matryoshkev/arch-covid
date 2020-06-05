# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("stringr")
library("ggplot2")
# library("gtable")
library("rmarkdown")

# Load data
data_cases <- read_csv("data/data-cases.csv")
data_testing <- read_csv("data/data-testing.csv")

# Find population sizes
data_population <- data_cases %>%
	filter(!is.na(CBSA_title)) %>%
	select(CBSA_code, CBSA_title, population) %>%
	group_by(CBSA_code, CBSA_title) %>%
	summarize(population = sum(population)) 

# Select largest 20 statistical areas
data_population <- data_population %>%
	arrange(desc(population)) %>%
	ungroup() %>%
	slice_head(n = 20)
# data_population  # Includes St Louis (CBSA_code == 41180)
CBSA_st_louis <- 41180

# Find new cases
avg_period <- 7  # How many days to average over
data_metros <- data_cases %>% 
	filter(CBSA_code %in% unique(data_population$CBSA_code)) %>%
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	group_by(CBSA_code, date) %>%
	summarize(cases = sum(cases, na.rm = TRUE)) %>%
	left_join(data_population) %>%
	mutate(date = ymd(date)) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		new_cases = (cases - lag(cases, n = avg_period)) / avg_period, 
		cases_per_capita = cases / population, 
		new_cases_per_capita = new_cases / population, 
		focus_area = if_else(CBSA_code == CBSA_st_louis, "St. Louis", "Other US metro areas")
	)

# Find testing effort
data_testing <- data_testing %>%
	filter(state %in% c("Missouri", "Illinois")) %>%
	group_by(state_abbrev) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		positive_new  = positive - lag(positive, n = avg_period), 
		total_new     = total - lag(total, n = avg_period), 
		rate_positive = positive_new / total_new, 
		rate_negative = 1 - rate_positive
	) %>%
	filter(!is.na(rate_positive))

# Define shared plot elements
my_theme <- theme(
	text = element_text(size = 10), 
	axis.title.x = element_blank(), 
	legend.title = element_blank(), 
	legend.margin = margin(0, 0, 0, 0, "in"), 
	# plot.caption = element_text(hjust = 0)
)
make_log_labels <- function(x) {
	if (x == 1) { 
		as.expression(1) 
	} else {
		as.expression(bquote(10^.(log10(x))))
		# as.expression(bquote(paste(" "[10], .(log10(x)))))
	}
}
plot_base <- ggplot() + 
	# geom_line(size = 0.5) + 
	scale_x_date(limits = c(ymd("2020-03-01"), NA), date_labels = "%b %e") + 
	scale_y_log10(
		breaks = 10^c(-9:0), minor_breaks = 10^c(-9:0), 
		labels = sapply(10^c(-9:0), make_log_labels)
	) + 
	my_theme 


# Plot new cases
plot_new_cases <- plot_base %+% 
	subset(data_metros, new_cases_per_capita > 0) + 
	aes(x = date, y = new_cases_per_capita, group = CBSA_code, color = focus_area) + 
	geom_line(size = 0.5) + 
	geom_line(data = subset(data_metros, CBSA_code == CBSA_st_louis), size = 0.75) + 
	scale_color_manual(values = c(grey(0.8), "red2")) +
	labs(y = paste0("New cases per person\n(log scale, ", avg_period, " day average) ")) + 
	theme(legend.position = "top")
ggsave("docs/images/STL-new-cases.png", width = 5, height = 3)

# Plot cumulative cases
plot_cumulative_cases <- plot_base %+% 
	subset(data_metros, cases_per_capita > 0) + 
	aes(x = date, y = cases_per_capita, group = CBSA_title, color = focus_area) + 
	geom_line(size = 0.5) + 
	geom_line(data = subset(data_metros, CBSA_code == CBSA_st_louis), size = 0.75) + 
	scale_color_manual(values = c(grey(0.8), "red2")) +
	labs(y = "Cumulative cases per person\n(log scale)") + 
	theme(legend.position = "top")
ggsave("docs/images/STL-cumulative.png", width = 5, height = 3)

# if (dev.cur() < 2) {dev.new(width = 5, height = 3)}
# plot(plot_new_cases)

# Plot testing effort
plot_testing <- ggplot() %+% 
	data_testing + 
	aes(x = date, y = rate_negative, color = state) + 
	geom_line(size = 0.75) + 
	scale_x_date(limits = c(ymd("2020-03-01"), NA), date_labels = "%b %e") + 
	scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5)) + 
	labs(y = paste("Testing effort\n(fraction negative,", avg_period, "day average)")) + 
	my_theme + theme(
		legend.title = element_blank(), 
		legend.position = c(0.97, 0.03), 
		legend.justification = c(1, 0), 
		legend.background = element_blank(), 
		legend.direction = "horizontal"
	)
ggsave("docs/images/STL-testing.png", width = 5, height = 3)



# Update web report
rmarkdown::render("docs/index.Rmd", quiet = TRUE)

# Clean up
# rm(data_cases, data_population, data_metro, avg_period)
# rm(my_theme, make_log_labels, plot_base, plot_cumulative_cases, plot_new_cases, plot_combined)

