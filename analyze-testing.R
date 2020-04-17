# Analyze case testing effort

# TO DO: These are CUMULATIVE counts


# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("scales")
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
lag_time <- 7 # days
data_testing <- data_testing %>%
	group_by(state) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		positive_new  = positive - lag(positive, n = lag_time), 
		total_new     = total - lag(total, n = lag_time), 
		rate_positive = positive_new / total_new
	)

# Plot testing data
my_theme <- theme(
	text = element_text(size = 10), 
	axis.title.x = element_blank(), 
	strip.background = element_blank()
)
plot_count <- data_testing %>% 
	select(date, state, positive, negative) %>%
	pivot_longer(cols = c(positive, negative), names_to = "result", values_to = "tests") %>% 
	mutate(result = relevel(factor(result), ref = "positive")) %>%
	filter(tests > 0) %>%
	ggplot(mapping = aes(x = date, y = tests, color = result)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.75) + 
		scale_y_log10(labels = scales::trans_format("log10", math_format(10^.x))) + 
		labs(y = "Test count") + 
		my_theme + 
		theme(
			legend.title = element_blank(), 
			legend.position = c(0.5, 0), 
			legend.justification = c(1.15, -0.15), 
			strip.text = element_text(size = 11)
		)
plot_positive <- data_testing %>%
	filter(!is.na(rate_positive)) %>% 
	ggplot(mapping = aes(x = date, y = rate_positive)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.75) + 
		scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + 
		labs(y = paste("Fraction positive in last", lag_time, "days")) + 
		my_theme + 
		theme(strip.text = element_blank())
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
ggsave("results/testing.pdf", plot = plot_testing, width = 6, height = 5)

# MO didn't start reporting good negative test numbers until 2020-03-28

# Clean up
rm(data_testing, lag_time, my_theme, plot_count, plot_positive, plot_testing)

