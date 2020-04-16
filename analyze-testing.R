# Analyze case testing effort

# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("scales")
library("gtable")

# Read and format testing data
testing_data <- read_csv("data/data-testing.csv") %>% 
	filter(state %in% c("MO", "IL")) %>%
	mutate(state = case_when(
			state == "MO" ~ "Missouri", 
			state == "IL" ~ "Illinois"
	)) %>%
	mutate(state = relevel(factor(state), ref = "Missouri")) %>%
	mutate(fraction_positive = positive / total)

# Plot testing data
my_theme <- theme(
	text = element_text(size = 10), 
	axis.title.x = element_blank(), 
	strip.background = element_blank()
)
plot_count <- testing_data %>% 
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
plot_fraction_positive <- testing_data %>%
	filter(!is.na(fraction_positive)) %>% 
	ggplot(mapping = aes(x = date, y = fraction_positive)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.75) + 
		scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + 
		labs(y = "Fraction positive") + 
		my_theme + 
		theme(strip.text = element_blank())
pdf(file = NULL)  
	# workaround so that ggplotGrob() doesn't create a blank plot window
	# see https://github.com/tidyverse/ggplot2/issues/809
plot_testing <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 7), "null")), 
	list(ggplotGrob(plot_count), ggplotGrob(plot_fraction_positive)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 5), b = c(4, 7)
)
dev.off()  # end of workaround
# dev.new(width = 6, height = 5)
# plot(plot_testing)
ggsave("results/testing.pdf", plot = plot_testing, width = 6, height = 5)

# MO didn't start reporting good negative test numbers until 2020-03-28
# IL testing effort stabilizes at 2020-03-29

# Clean up
rm(testing_data, my_theme, plot_count, plot_fraction_positive, plot_testing)

