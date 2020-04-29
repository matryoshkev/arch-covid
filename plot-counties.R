# Plot SARS-CoV-2/COVID-19 activity
# by county for select metropolitan areas

# TO DO: 
# - order counties by population?
# - show when cases below axis limit?

# Dependencies
library("readr")
library("dplyr")
library("lubridate")
library("tidyr")
library("ggplot2")
library("gtable")


# my_metro <- "Atlanta"
 my_metro <- "Chicago"
# my_metro <- "Detroit"
# my_metro <- "New Orleans"
# my_metro <- "St. Louis"

# Case data
lag_period <- 5  # How many days to average over
data_cases <- read_csv("data/data-cases.csv") %>%
	filter(metro == my_metro) %>% 
	select(county, state, contains("-")) %>% 
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	mutate(date = ymd(date)) %>%
	group_by(county) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(new_cases = (cases - lag(cases, n = lag_period)) / lag_period)

# Plot data by county
my_theme <- theme(
	text = element_text(size = 9), 
	axis.title.x = element_blank(), 
	legend.position = "bottom", 
	plot.caption = element_text(hjust = 0), 
	strip.background = element_blank()
)
# my_minor_breaks <- c(seq(2, 10, by = 2), seq(2, 10, by = 2) * 10, seq(2, 10, by = 2) * 100)
plot_new_cases <- data_cases %>% 
	filter(cases > 0, !is.na(new_cases), new_cases > 2) %>%
	ggplot(mapping = aes(x = date, y = new_cases, color = county)) + 
		geom_line() + geom_point(size = 0.5) + 
		scale_x_date(
			# limits = c(min(filter(data_cases, cases > 0)$date), max(data_cases$date)), 
			limits = c(ymd("2020-03-15"), max(data_cases$date)), 
			date_labels = "%b %e"
		) + 
		scale_y_log10(
			breaks = 10^c(1:4), 
			minor_breaks = c(seq(2, 10, by = 2), seq(2, 10, by = 2) * 10, seq(2, 10, by = 2) * 100)
		) + 
		scale_color_hue(l = 50) + 
		labs(
			title = paste("COVID-19 activity:", my_metro, "metro area"), 
			y = paste("New confirmed cases\n(average of past", lag_period, "days)")
		) + 
		my_theme
# if (dev.cur() < 2) {dev.new(width = 5.5, height = 4)}
# plot(plot_new_cases)


# Testing data
data_testing <- read_csv("data/data-testing.csv") %>% 
	mutate(state = case_when(
		state == "GA" ~ "Georgia", 
		state == "LA" ~ "Louisiana", 
		state == "MI" ~ "Michigan", 
		state == "MO" ~ "Missouri", 
		state == "IL" ~ "Illinois"
	)) %>%
	filter(state %in% unique(data_cases$state)) %>%
	group_by(state) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		positive_new  = positive - lag(positive, n = lag_period), 
		total_new     = total - lag(total, n = lag_period), 
		rate_positive = positive_new / total_new
	) %>%
	filter(!is.na(rate_positive))
plot_testing <- data_testing %>%
	ggplot(mapping = aes(x = date, y = 1 - rate_positive, color = state)) + 
		geom_line() + geom_point(size = 0.75) + 
		scale_x_date(limits = date_limits, date_labels = "%b %e") + 
		scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5)) + 
		labs(
			# y = paste("Fraction tests positive\n", "(past", lag_period, "days)"), 
			y = paste("Testing intensity (fraction\nnegative over past", lag_period, "days)"), 
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
# dev.new(width = 6, height = 5)
# plot(plot_testing)

# Combined plot
pdf(file = NULL)  # start bug workaround
plot_activity <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 19), "null")), 
	list(ggplotGrob(plot_new_cases), ggplotGrob(plot_testing)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 13), b = c(12, 19)
)
dev.off()  # end bug workaround

# dev.new(width = 5, height = 6)
# plot(plot_activity)

ggsave(
	paste0("results/activity-", my_metro, ".pdf"), 
	plot = plot_activity, width = 5, height = 6
)



# ==== OLD STUFF BELOW ====

# ggsave("results/stl-activity.png", plot = plot_activity, width = 6, height = 5, units = "in")

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
rm(lag_period, data_cases, data_testing)
rm(my_theme, plot_new_cases, plot_testing, plot_activity)



