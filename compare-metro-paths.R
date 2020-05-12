# SARS-CoV-2/COVID-19 activity in select metropolitan areas

# TO DO: 
# - Caption for plot(s)
# ? measure & plot change in new cases (exponential rate)

# Dependencies
library("readr")
library("dplyr")
library("lubridate")
library("tidyr")
library("ggplot2")
library("gtable")

# Load data
data_cases <- read_csv("data/data-cases.csv")

# Find metro population sizes
data_population <- data_cases %>%
	filter(!is.na(metro)) %>%
	select(metro, population, county) %>%
	group_by(metro) %>%
	summarize(population = sum(population))

# Find new cases
avg_period <- 7  # How many days to average over
data_metro <- data_cases %>% 
	filter(!is.na(metro)) %>%
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	group_by(metro, date) %>%
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
	text = element_text(size = 9), 
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
	}
}
plot_base <- ggplot() + 
	geom_line(size = 0.75) + 
	scale_x_date(limits = c(ymd("2020-03-01"), NA), date_labels = "%b %e") + 
	scale_y_log10(
		breaks = 10^c(-9:0), minor_breaks = 10^c(-9:0), 
		labels = sapply(10^c(-9:0), make_log_labels)
	) + 
	scale_color_hue(l = 60) + 
	my_theme 

# Plot data
plot_cumulative_cases <- plot_base %+% 
	subset(data_metro, cases_per_capita > 0) + 
	aes(x = date, y = cases_per_capita, color = metro) + 
	labs(y = "Cumulative cases per capita\n") + 
	theme(legend.position = "top")
plot_new_cases <- plot_base %+% 
	subset(data_metro, new_cases_per_capita > 0) + 
	aes(x = date, y = new_cases_per_capita, color = metro) + 
	labs(y = paste0("New cases per capita\n(", avg_period, " day avg.) ")) + 
	theme(legend.position = "none")
pdf(file = NULL)  # start bug workaround
plot_combined <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 19), "null")), 
	list(ggplotGrob(plot_cumulative_cases), ggplotGrob(plot_new_cases)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 13), b = c(12, 19)
)
dev.off()  # end bug workaround
if (dev.cur() < 2) {dev.new(width = 5, height = 6)}
plot(plot_combined)

ggsave("results/metro-paths.pdf", plot = plot_combined, width = 5, height = 6)

# Clean up
rm(data_cases, data_population, data_metro, avg_period)
rm(my_theme, make_log_labels, plot_base, plot_cumulative_cases, plot_new_cases, plot_combined)


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

