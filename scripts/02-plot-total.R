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
	filter(
		((state == "Missouri") & county %in% c(
			"St. Louis City", "St. Louis", "St. Charles", 
			"Lincoln", "Warren", "Franklin", "Jefferson"
		)) | 
		((state == "Illinois") & county %in% c(
			"Madison", "St. Clair", "Monroe", "Jersey", "Clinton"
		))
	) %>%
	select(-c(UID, county_label)) %>% 
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	mutate(date = ymd(date)) %>%
	group_by(date) %>%
	summarise(cases = sum(cases, na.rm = TRUE)) %>%
	mutate(new_cases = cases - lag(cases, n = 1)) %>%
	filter(date > ymd("2020-03-01"))

# Plot data
my_theme <- theme(text = element_text(size = 10), axis.title.x = element_blank())
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
			"\n Data from Johns Hopkins University CSSE\n", 
			"Plot by jeff smith Ph.D. https://github.com/matryoshkev/arch-covid"
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
plot(plot_metro)
ggsave("results/stl-total.pdf", plot_metro, device = "pdf", width = 5.5, height = 5)

# Clean up
rm(data_metro, my_theme, plot_cases, plot_new_cases, plot_metro)



