# Analyze case testing effort

# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("forcats")
library("ggplot2")
# library("scales")
library("gtable")

# Read and format testing data
data_testing <- read_csv("data/data-testing.csv") %>% 
	filter(state %in% c("MO", "IL")) %>%
	mutate(state = case_when(
		state == "MO" ~ "Missouri", 
		state == "IL" ~ "Illinois"
	)) %>%
	mutate(state = relevel(factor(state), ref = "Missouri"))

# Calculate fraction positive in last X days
lag_time <- 5  # days
data_testing <- data_testing %>%
	group_by(state) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		positive_new  = (positive - lag(positive, n = lag_time)) / lag_time, 
		negative_new  = (negative - lag(negative, n = lag_time)) / lag_time, 
		total_new     = (total - lag(total, n = lag_time)) / lag_time, 
		rate_positive = positive_new / total_new
	)

# Plot testing data
my_theme <- theme(
	text = element_text(size = 9), axis.title.x = element_blank(), 
	# strip.text = element_text(size = 8) 
	strip.background = element_blank()
)
plot_count <- data_testing %>% 
	select(date, state, positive_new, negative_new) %>%
	pivot_longer(cols = c(positive_new, negative_new), names_to = "result", values_to = "tests") %>% 
	mutate(
		result = case_when(
			result == "positive_new" ~ "positive", 
			result == "negative_new" ~ "negative"
		), 
		result = relevel(factor(result), ref = "positive")
	) %>%
	filter(!is.na(tests)) %>%
	ggplot(mapping = aes(x = date, y = tests, color = result)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.75) + 
		scale_x_date(date_labels = "%b %e") + 
		# scale_y_log10(labels = scales::trans_format("log10", math_format(10^.x))) + 
		scale_y_log10(limits = c(0.2, 5e4), minor_breaks = 10^c(1:5)) + 
		labs(y = paste("Test count (average of last", lag_time, "days)")) + 
		my_theme + 
		theme(
			legend.title = element_blank(), 
			legend.position = c(0.5, 0), 
			legend.justification = c(1.15, -0.15), 
			strip.text = element_text(size = 10)
		)
plot_positive <- data_testing %>%
	filter(!is.na(rate_positive)) %>% 
	ggplot(mapping = aes(x = date, y = rate_positive)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.75) + 
		scale_x_date(date_labels = "%b %e") + 
		scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + 
		labs(
			y = paste("Fraction positive\nin last", lag_time, "days"), 
			caption = "\nData from The COVID Tracking Project (https://covidtracking.com)"
		) + 
		my_theme + theme(strip.text = element_blank())
pdf(file = NULL)  
	# workaround so that ggplotGrob() doesn't create a blank plot window
	# see https://github.com/tidyverse/ggplot2/issues/809
plot_testing <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 7), "null")), 
	list(ggplotGrob(plot_count), ggplotGrob(plot_positive)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 5), b = c(4, 7)
)
dev.off()  # end of workaround
# dev.new(width = 6, height = 5)
# plot(plot_testing)
ggsave("results/stl-testing.pdf", plot = plot_testing, width = 6, height = 5)

# qplot(x = date, y = total_new, color = state, data = data_testing, log = "y")
# data_testing %>%
	# filter(!is.na(rate_positive)) %>% 
	# ggplot(mapping = aes(x = date, y = total_new)) + 
		# facet_wrap(~ state, ncol = 2) + 
		# scale_x_date(date_labels = "%b %e") + 
		# scale_y_log10() + 
		# geom_line() + geom_point(size = 0.75) + 
		# labs(y = paste("Test count\n(average of last", lag_time, "days)")) + 
		# my_theme

# MO didn't start reporting good negative test numbers until 2020-03-28

# Clean up
rm(data_testing, lag_time, my_theme, plot_count, plot_positive, plot_testing)

