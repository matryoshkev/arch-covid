# SARS-CoV-2/COVID-19 activity in select metropolitan areas

# TO DO: 
# - Include testing data
# ? Captions
# ? Measure & plot change in new cases (exponential rate)

# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("stringr")
library("ggplot2")
library("gtable")

# Load data
data_cases <- read_csv("data/data-cases.csv")

# Select  Core-Based Statistical Areas (CBSA) to focus on
# data_population %>% filter(str_detect(CBSA_title, "Minneapolis"))
focus_areas <- c(
	# CBSA_code CBSA_title
	12060, # Atlanta
	14460, # Boston
	16980, # Chicago
	19100, # Dallas-Fort Worth
	19820, # Detroit
	26420, # Houston
	31080, # Los Angeles
	33100, # Miami
	33460, # Minneapolis
	35380, # New Orleans
	35620, # New York CIty
	38060, # Phoenix
	37980, # Philadelphia
	41180, # St. Louis
	42660, # Seattle
	41860, # SF Bay Area
	47900  # Washington DC
)
# data_cases %>%
	# filter(CBSA_code %in% focus_areas) %>%
	# select(CBSA_title) %>%
	# distinct()

# Find population sizes for all CBSAs
# data_population <- data_cases %>%
	# filter(!is.na(metro)) %>%
	# select(metro, population, county) %>%
	# group_by(metro) %>%
	# summarize(population = sum(population))
data_population <- data_cases %>%
	filter(!is.na(CBSA_title)) %>%
	select(CBSA_code, CBSA_title, population) %>%
	group_by(CBSA_code, CBSA_title) %>%
	summarize(population = sum(population))


# Find new cases
avg_period <- 7  # How many days to average over
data_metro <- data_cases %>% 
	filter(CBSA_code %in% focus_areas) %>%
	# filter(!is.na(metro)) %>%
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	# group_by(metro, date) %>%
	group_by(CBSA_code, date) %>%
	summarize(cases = sum(cases, na.rm = TRUE)) %>%
	left_join(data_population) %>%
	mutate(date = ymd(date)) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		new_cases = (cases - lag(cases, n = avg_period)) / avg_period, 
		cases_per_capita = cases / population, 
		new_cases_per_capita = new_cases / population
		# new_cases_increase = new_cases / lag(new_cases, n = avg_period)
	)

# Define plot elements
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
# plot_base <- ggplot() + 
	# geom_line(size = 0.75) + 
	# scale_x_date(limits = c(ymd("2020-03-01"), NA), date_labels = "%b %e") + 
	# scale_y_log10(
		# breaks = 10^c(-9:0), minor_breaks = 10^c(-9:0), 
		# labels = sapply(10^c(-9:0), make_log_labels)
	# ) + 
	# scale_color_hue(l = 60) + 
	# my_theme 

# # Plot data
# plot_cumulative_cases <- plot_base %+% 
	# subset(data_metro, cases_per_capita > 0) + 
	# aes(x = date, y = cases_per_capita, color = metro) + 
	# labs(y = "Cumulative cases per capita\n") + 
	# theme(legend.position = "top")
# plot_new_cases <- plot_base %+% 
	# subset(data_metro, new_cases_per_capita > 0) + 
	# aes(x = date, y = new_cases_per_capita, color = metro) + 
	# labs(y = paste0("New cases per capita\n(", avg_period, " day avg.) ")) + 
	# theme(legend.position = "none")
# pdf(file = NULL)  # start bug workaround
# plot_combined <- gtable_add_grob(
	# gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 19), "null")), 
	# list(ggplotGrob(plot_cumulative_cases), ggplotGrob(plot_new_cases)), 
	# l = c(1, 1), r = c(1, 1), t = c(1, 13), b = c(12, 19)
# )
# dev.off()  # end bug workaround
# if (dev.cur() < 2) {dev.new(width = 5, height = 6)}
# plot(plot_combined)

# ggsave("results/metro-paths.pdf", plot = plot_combined, width = 5, height = 6)


# A different way: 

plot_base <- ggplot() + 
	geom_line(size = 0.5, color = gray(0.75)) + 
	scale_x_date(limits = c(ymd("2020-03-01"), NA), date_labels = "%b %e") + 
	scale_y_log10(
		breaks = 10^c(-9:0), minor_breaks = 10^c(-9:0), 
		labels = sapply(10^c(-9:0), make_log_labels)
	) + 
	my_theme 
plot_new_cases <- plot_base %+% 
	subset(data_metro, new_cases_per_capita > 0) + 
	# aes(x = date, y = new_cases_per_capita, group = metro) + 
	aes(x = date, y = new_cases_per_capita, group = CBSA_title) + 
	labs(y = paste0("New cases per capita\n(", avg_period, " day avg.) ")) + 
	theme(legend.position = "top")
plot_cumulative_cases <- plot_base %+% 
	subset(data_metro, cases_per_capita > 0) + 
	# aes(x = date, y = cases_per_capita, group = metro) + 
	aes(x = date, y = cases_per_capita, group = CBSA_title) + 
	labs(y = "Cumulative cases per capita\n") + 
	theme(legend.position = "none")

# for (selected_metro in unique(data_metro$metro)) {
for (selected_metro in unique(data_metro$CBSA_title)) {
	cat(selected_metro, "...\n")
	plot_cumulative_focus <- plot_cumulative_cases + 
		geom_line(data = subset(data_metro, CBSA_title == selected_metro), size = 0.75) + 
		aes(color = CBSA_title) + scale_color_manual(values = c("red2"))
	plot_new_focus <- plot_new_cases + 
		geom_line(data = subset(data_metro, CBSA_title == selected_metro), size = 0.75) + 
		aes(color = CBSA_title) + scale_color_manual(values = c("red2"))
	pdf(file = NULL)  # start bug workaround
	plot_combined_focus <- gtable_add_grob(
		gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 19), "null")), 
		list(ggplotGrob(plot_new_focus), ggplotGrob(plot_cumulative_focus)), 
		l = c(1, 1), r = c(1, 1), t = c(1, 11), b = c(10, 19)
	)
	dev.off()  # end bug workaround
	ggsave(
		paste0("results/path-", str_extract(selected_metro, "^[a-zA-Z.\\s]+"), ".png"), 
		plot = plot_combined_focus, width = 5, height = 6
	)
}
# if (dev.cur() < 2) {dev.new(width = 5, height = 6)}
# plot(plot_combined_focus)

# str_extract("St. Primary City-others-maybe, ST", "^[.a-zA-Z\\s]+") 


# Clean up
# rm(data_cases, data_population, data_metro, avg_period)
# rm(my_theme, make_log_labels, plot_base, plot_cumulative_cases, plot_new_cases, plot_combined)


# plot_new_cases_increase <- subset(data_metro, is.finite(new_cases_increase)) %>%
	# ggplot() + 
	# aes(x = date, y = new_cases_increase, color = metro) + 
	# geom_line(size = 0.75) + 
	# scale_x_date(limits = c(ymd("2020-03-01"), NA), date_labels = "%b %e") + 
	# scale_y_continuous(
		# # breaks = 10^c(-9:0), minor_breaks = 10^c(-9:0), 
		# # labels = sapply(10^c(-9:0), make_log_labels)
	# ) + 
	# scale_color_hue(l = 60) + 
	# my_theme


# caption = paste0(
	# "\n", "Cases are time-delayed undercount of total infections. ", 
	# "Case testing effort varies among states and over time. ". 
	# "Cases are better indicator of virus activity\nwhen positive test rate is stable. ", 
	# "Data from Johns Hopkins CSSE. ", 
	# "Data from Johns Hopkins CSSE and The COVID Tracking\nProject. ", 
	# "Plot by jeff smith Ph.D. https://github.com/matryoshkev/arch-covid"
# )

