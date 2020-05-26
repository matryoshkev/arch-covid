# SARS-CoV-2/COVID-19 activity in select Core-Based Statistical Areas (CBSAs)

# TO DO: 
# - Clean up names and objects: plots, data, avg periods, etc
# - Total tests vs time?

# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("stringr")
library("ggplot2")
library("gtable")

# Data
data_cases   <- read_csv("data/data-cases.csv")
data_testing <- read_csv("data/data-testing.csv")

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
	37980, # Philadelphia
	38060, # Phoenix
	41180, # St. Louis
	42660, # Seattle
	41860, # SF Bay Area
	47900  # Washington DC
)

# Parameters
avg_period <- 7  # How many days to average over
min_cases  <- 1
min_date   <- ymd("2020-03-08")

label_cumulative <- "Cumulative cases"
label_new_cases  <- paste0("New cases (", avg_period, " day avg.) ")

# Plot elements
my_theme <- theme(
	text = element_text(size = 7), 
	axis.title.x = element_blank(), 
	strip.background = element_blank(), 
	title = element_text(size = 8), 
	legend.margin = margin(0, 0, 0, 0, "in"), 
	legend.title = element_blank(), 
	plot.caption = element_text(hjust = 0)
)
make_log_labels <- function(x) {
	if (x == 1) { 
		as.expression(1) 
	} else {
		as.expression(bquote(10^.(log10(x))))
	}
}
plot_cases_base <- ggplot() + 
	geom_line() + geom_point(size = 0.5) + scale_color_hue(l = 50) + 
	scale_x_date(limits = c(min_date, NA), date_labels = "%b %e") + 
	scale_y_log10(
		limits = c(min_cases, NA), 
		breaks = 10^c(0:6), minor_breaks = 10^c(0:6), 
		labels = sapply(10^c(0:6), make_log_labels)
	) + 
	my_theme + theme(legend.position = "bottom")
plot_cases_per_capita_base <- ggplot() + 
	geom_line() + geom_point(size = 0.5) + scale_color_hue(l = 50) + 
	scale_x_date(limits = c(min_date, NA), date_labels = "%b %e") + 
	scale_y_log10(
		breaks = 10^c(-8:0), minor_breaks = 10^c(-8:0), 
		labels = sapply(10^c(-8:0), make_log_labels)
	) + 
	my_theme + theme(legend.position = "bottom")

plot_testing_base <- ggplot(mapping = aes(x = date, y = rate_negative, color = state)) + 
	geom_line() + geom_point(size = 0.75) + 
	scale_x_date(limits = c(min_date, NA), date_labels = "%b %e") + 
	scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5)) + 
	labs(
		y = paste("Testing effort\n(fraction negative,", avg_period, "day avg.)"), 
		caption = paste0(
			"\n", "Cases are time-delayed undercount of total infections. ", 
			"Cases are better indicator of virus activity\nwhen positive test rate is stable. ", 
			"Data from Johns Hopkins CSSE and The COVID Tracking\nProject. ", 
			"Plot by jeff smith Ph.D. https://github.com/matryoshkev/arch-covid"
		)
	) + 
	my_theme + theme(
		legend.title = element_blank(), 
		legend.position = c(0.97, 0.03), 
		legend.justification = c(1, 0), 
		legend.background = element_blank(), 
		legend.direction = "horizontal"
	)

# Plotting function
make_plots <- function(selected_CBSA_code) {

	selected_CBSA_title <- data_cases %>% 
		filter(CBSA_code == selected_CBSA_code) %>%
		select(CBSA_title) %>% 
		distinct()
	selected_CBSA_title <- selected_CBSA_title[[1]]
	cat(selected_CBSA_title, "...\n")

	selected_data_CBSA <- data_cases %>%
		filter((CBSA_code == selected_CBSA_code) & (central_or_outlying_county == "Central")) %>% 
		select(county, state, population, contains("-")) %>% 
		pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
		mutate(date = ymd(date))

	selected_cases <- selected_data_CBSA %>%
		mutate(
			# Condense county names for legend
			county = str_remove(county, " County$"), 
			county = str_remove(county, " Parish$"), 
		) %>%
		group_by(county) %>%
		arrange(date, .by_group = TRUE) %>%
		mutate(
			new_cases = (cases - lag(cases, n = avg_period)) / avg_period, 
			new_cases_per_capita = new_cases / population, 
			new_cases = if_else(new_cases < min_cases, 0, new_cases)
		) %>%
		filter(!is.na(new_cases))

	# population_CBSA <- selected_data_CBSA %>% 
		# select(county, state, population) %>% 
		# distinct() %>%
		# summarise(population = sum(population, na.rm = TRUE))
	# population_CBSA <- population_CBSA[[1]]

	# selected_data_CBSA <- selected_data_CBSA %>% 
		# group_by(date) %>%
		# summarise(cases = sum(cases, na.rm = TRUE)) %>%
		# mutate(new_cases = (cases - lag(cases, n = avg_period)) / avg_period) %>%
		# pivot_longer(cols = c(cases, new_cases), names_to = "case_type", values_to = "count") %>%
		# mutate(
			# case_type = case_when(
				# case_type == "cases"     ~ label_cumulative, 
				# case_type == "new_cases" ~ label_new_cases
			# ), 
			# case_type = factor(case_type, levels = c(label_new_cases, label_cumulative)), 
			# count = if_else(count < 1, 0, count), 
			# count_per_capita = count / population_CBSA
		# )

	selected_testing <- data_testing %>%
		filter(state %in% unique(selected_cases$state)) %>%
		group_by(state) %>%
		arrange(date, .by_group = TRUE) %>%
		mutate(
			positive_new  = positive - lag(positive, n = avg_period), 
			total_new     = total - lag(total, n = avg_period), 
			rate_positive = positive_new / total_new, 
			rate_negative = 1 - rate_positive
		) %>%
		filter(!is.na(rate_positive))

	# plot_CBSA <- selected_data_CBSA %>%
		# ggplot(mapping = aes(x = date, y = count, color = case_type)) + 
		# geom_point(size = 0.75) + geom_line() + 
		# scale_x_date(limits = c(min_date, NA), date_labels = "%b %e") + 
		# scale_y_log10(
			# limits = c(1, NA), breaks = 10^c(0:8), minor_breaks = 10^c(0:6), 
			# labels = sapply(10^c(0:8), make_log_labels)
		# ) + 
		# scale_color_manual(values = c("red", grey(0.5))) + 
		# labs(
			# y = "Case count\n", 
			# subtitle = paste("SARS-CoV-2 activity:", selected_CBSA_title)
		# ) + 
		# my_theme + theme(
			# # axis.title.y = element_blank(), 
			# legend.background = element_blank(), 
			# # legend.position = "bottom", 
			# # legend.position = c(0, 1), 
			# # legend.justification = c(-0.05, 1.1), 
			# legend.position = c(1, 0), 
			# legend.justification = c(1.1, -0.1), 
			# legend.title = element_blank(), 
			# title = element_text(size = 9) 
		# )

	# plot_CBSA_per_capita <- selected_data_CBSA %>%
		# ggplot(mapping = aes(x = date, y = count_per_capita, color = case_type)) + 
		# geom_point(size = 0.75) + geom_line() + 
		# scale_x_date(
			# limits = c(min_date, NA), date_labels = "%b %e" 
			# # date_breaks = "2 weeks", minor_breaks = "1 week"
		# ) + 
		# scale_y_log10(
			# breaks = 10^c(-8:8), minor_breaks = 10^c(-8:8), 
			# labels = sapply(10^c(-8:8), make_log_labels)
		# ) + 
		# scale_color_manual(values = c("red", grey(0.5))) + 
		# labs(
			# y = "Cases per capita\n", 
			# subtitle = ""
		# ) + 
		# my_theme + theme(
			# # axis.title.y = element_blank(), 
			# legend.background = element_blank(), 
			# # legend.position = "bottom", 
			# # legend.position = c(0, 1), 
			# # legend.justification = c(-0.05, 1.1), 
			# legend.position = c(1, 0), 
			# legend.justification = c(1.1, -0.1), 
			# legend.title = element_blank(), 
			# title = element_text(size = 9) 
		# )

	plot_cases <- plot_cases_base %+% selected_cases + 
		aes(x = date, y = new_cases, color = county) + 
		labs(
			subtitle = paste("SARS-CoV-2 activity:", selected_CBSA_title), 
			y = paste0("New confirmed cases\n(", avg_period, "day avg.)")
		)
	plot_cases_per_capita <- plot_cases_per_capita_base %+% selected_cases + 
		aes(x = date, y = new_cases_per_capita, color = county) + 
		labs(
			subtitle = "", 
			y = paste0("New confirmed cases per capita\n(", avg_period, "day avg.)")
		)
	plot_testing <- plot_testing_base %+% selected_testing

	pdf(file = NULL)  # start bug workaround
	plot_combined <- gtable_add_grob(
		# gtable(widths = unit(rep(1, 2), "null"), heights = unit(rep(1, 28), "null")), 
		gtable(widths = unit(rep(1, 2), "null"), heights = unit(rep(1, 21), "null")), 
		list(
			# ggplotGrob(plot_CBSA), 
			# ggplotGrob(plot_CBSA_per_capita), 
			ggplotGrob(plot_cases), 
			ggplotGrob(plot_cases_per_capita), 
			ggplotGrob(plot_testing)
		), 
		# l = c(1, 2, 1, 2, 1), r = c(1, 2, 1, 2, 1), t = c(1, 1, 10, 10, 21), b = c(9, 9, 20, 20, 28)
		l = c(1, 2, 1), r = c(1, 2, 1), t = c(1, 1, 12), b = c(11, 11, 21)
	)
	dev.off()  # end bug workaround

	ggsave(
		paste0("results/counties-", str_extract(selected_CBSA_title, "^[a-zA-Z.\\s]+"), ".pdf"), 
		plot = plot_combined, width = 7, height = 6
	)
}

for (selected_CBSA_code in focus_areas) {
	make_plots(selected_CBSA_code)
}


# Clean up
# rm(data_cases, data_testing)
# rm(avg_period, min_cases, min_date)
# rm(my_theme, plot_cases_base, plot_testing_base, make_plots)

