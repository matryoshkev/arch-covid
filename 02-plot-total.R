# Plot confirmed cases for total St Louis metropolitan area

# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("gtable")

# Read and format data
data_metro <- read_csv("data/data-cases.csv") %>%
	filter(metro == "St. Louis") %>%
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	mutate(date = ymd(date)) %>%
	group_by(date) %>%
	summarise(cases = sum(cases, na.rm = TRUE)) %>%
	mutate(new_cases = cases - lag(cases, n = 1)) %>%
	filter(date > ymd("2020-03-01"))

# Plot data
my_theme <- theme(text = element_text(size = 9), axis.title.x = element_blank())
plot_cases <- data_metro %>%
	ggplot(mapping = aes(x = date, y = cases)) + 
	geom_col() + 
	labs(
		y = "Cumulative confirmed cases", 
		title = "Spread of COVID-19 in the St Louis metro area"
	) + 
	my_theme
plot_new_cases <- data_metro %>%
	ggplot(mapping = aes(x = date, y = new_cases)) + 
	geom_col() + 
	labs(
		y = "New confirmed cases", 
		caption = (paste0(
			"\n", "Data from Johns Hopkins University CSSE" 
			# "Plot by jeff smith Ph.D. https://github.com/matryoshkev/arch-covid"
		)) 
	) + 
	my_theme
pdf(file = NULL)  # start bug workaround
plot_metro <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 2), "null")), 
	# gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 13), "null")), 
	list(ggplotGrob(plot_cases), ggplotGrob(plot_new_cases)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 2), b = c(1, 2)
	# l = c(1, 1), r = c(1, 1), t = c(1, 7), b = c(6, 13)
)
dev.off()  # end bug workaround
# dev.new(width = 5.5, height = 5)
# plot(plot_metro)
ggsave("results/stl-total.pdf", plot_metro, device = "pdf", width = 5.5, height = 5)

# Trying something out: 
# avg_period <- 3
# label_cumulative <- "Cumulative cases"
# label_new_cases  <- paste0("New cases (", avg_period, " day average)")
# data_metro %>% 
	# mutate(new_cases = (cases - lag(cases, n = avg_period)) / avg_period) %>%
	# pivot_longer(cols = c(cases, new_cases), names_to = "case_type", values_to = "count") %>%
	# mutate(
		# case_type = case_when(
			# case_type == "cases"     ~ label_cumulative, 
			# case_type == "new_cases" ~ label_new_cases
		# ), 
		# case_type = factor(case_type, levels = c(label_new_cases, label_cumulative)), 
		# count = if_else(count < 1, 0, count)
	# ) %>%
	# ggplot(mapping = aes(x = date, y = count, color = case_type)) + 
		# geom_point(size = 0.75) + geom_line() + 
		# scale_x_date(date_breaks = "1 week", date_labels = "%b %e", minor_breaks = NULL) + 
		# scale_y_log10(limits = c(1, 1e4), minor_breaks = 10^c(0:6)) + 
		# scale_color_manual(values = c("red", grey(0.5))) + 
		# labs(y = "Case count", title = "COVID-19 activity: St. Louis metro area") + 
		# my_theme + theme(
			# axis.title.y = element_blank(), 
			# # legend.background = element_blank(), 
			# legend.position = c(0, 1), 
			# legend.justification = c(-0.05, 1.1), 
			# legend.title = element_blank(), 
			# title = element_text(size = 9) 
		# )

# Clean up
rm(data_metro, my_theme, plot_cases, plot_new_cases, plot_metro)



