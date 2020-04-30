# SARS-CoV-2/COVID-19 activity in select metropolitan areas

# TO DO: 
# - Plot cases per indiv? (plot_impact)
# - Plot new cases total metro area?
# - Plot cumulative cases (total metro)?


# Dependencies
library("readr")
library("dplyr")
library("lubridate")
library("tidyr")
library("ggplot2")
library("gtable")

# Data
data_cases   <- read_csv("data/data-cases.csv")
data_testing <- read_csv("data/data-testing.csv")

# Parameters
avg_period <- 7  # How many days to average over
min_cases  <- 4
min_date   <- ymd("2020-03-15")

# Plot elements
my_theme <- theme(
	text = element_text(size = 9), 
	axis.title.x = element_blank(), 
	strip.background = element_blank(), 
	title = element_text(size = 9), 
	legend.position = "bottom", 
	plot.caption = element_text(hjust = 0)
)
plot_cases_base <- ggplot(mapping = aes(x = date, y = new_cases, color = county)) + 
	geom_line() + geom_point(size = 0.5) + scale_color_hue(l = 50) + 
	scale_x_date(limits = c(min_date, NA), date_labels = "%b %e") + 
	scale_y_log10(
		limits = c(min_cases, NA), breaks = 10^c(1:5), 
		minor_breaks = c(
			seq(2, 10, by = 2), seq(2, 10, by = 2) * 10, 
			seq(2, 10, by = 2) * 100, seq(2, 10, by = 2) * 1000
		)
	) + 
	labs(y = paste("New confirmed cases\n(average of past", avg_period, "days)")) + 
	my_theme
plot_testing_base <- ggplot(mapping = aes(x = date, y = rate_negative, color = state)) + 
	geom_line() + geom_point(size = 0.75) + 
	scale_x_date(limits = c(min_date, NA), date_labels = "%b %e") + 
	scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5)) + 
	labs(
		y = paste("Testing intensity (fraction\nnegative over past", avg_period, "days)"), 
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
make_plots <- function(selected_metro) {

	selected_cases <- data_cases %>%
		filter(metro == selected_metro) %>% 
		select(county, state, contains("-")) %>% 
		pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
		mutate(date = ymd(date)) %>%
		group_by(county) %>%
		arrange(date, .by_group = TRUE) %>%
		mutate(
			new_cases = (cases - lag(cases, n = avg_period)) / avg_period, 
			new_cases = if_else(new_cases < min_cases, 0, new_cases)
		) %>%
		filter(!is.na(new_cases))

	selected_testing <- data_testing %>%
		mutate(state = case_when(
			state == "GA" ~ "Georgia", 
			state == "LA" ~ "Louisiana", 
			state == "MI" ~ "Michigan", 
			state == "MO" ~ "Missouri", 
			state == "IL" ~ "Illinois"
		)) %>%
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

	plot_cases <- plot_cases_base %+% selected_cases + 
		labs(title = paste("SARS-CoV-2 activity:", selected_metro, "metro area"))
	plot_testing <- plot_testing_base %+% selected_testing

	pdf(file = NULL)  # start bug workaround
	plot_combined <- gtable_add_grob(
		gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 19), "null")), 
		list(ggplotGrob(plot_cases), ggplotGrob(plot_testing)), 
		l = c(1, 1), r = c(1, 1), t = c(1, 13), b = c(12, 19)
	)
	dev.off()  # end bug workaround

	ggsave(
		paste0("results/activity-", selected_metro, ".pdf"), 
		plot = plot_combined, width = 5, height = 5.5
	)
	# return(plot_combined)
}

# dev.new(width = 5, height = 5.5)
# plot(make_plots("New Orleans"))

for (metro_area in c("St. Louis", "Atlanta", "Detroit", "New Orleans", "Chicago")) {
	make_plots(metro_area)
}



# ==== OLD STUFF BELOW ====


# Plot cumulative cases
# dev.new(width = 5.5, height = 3)
# plot_cases <- data_cases %>% 
	# filter(cases > 0) %>%
	# ggplot(mapping = aes(x = date, y = cases, color = county)) + 
		# facet_wrap(~ state, ncol = 2) + 
		# geom_line() + geom_point(size = 0.5) + 
		# scale_x_date(limits = date_limits) + 
		# scale_y_log10() + 
		# labs(y = "Cumulative confirmed cases") + 
		# my_theme
# plot(plot_cases)

# Clean up
rm(data_cases, data_testing)
rm(avg_period, min_cases, min_date)
rm(my_theme, plot_cases_base, plot_testing_base, make_plots)

