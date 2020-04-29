# Plot SARS-CoV-2/COVID-19 activity in St Louis metro area 

# TO DO: 
# - order counties by population
# - show when cases below axis limit?

# Dependencies
# library("readr")
# library("dplyr")
# library("lubridate")
# library("tidyr")
# library("ggplot2")
# library("gtable")

lag_period <- 5  # How many days to average over

# Case data
data_cases <- read_csv("data/data-cases.csv") %>%
	filter(
		((state == "Missouri") & county %in% c(
			"St. Louis City", "St. Louis", "St. Charles", 
			"Lincoln", "Warren", "Franklin", "Jefferson"
		)) | 
		((state == "Illinois") & county %in% c(
			"Madison", "St. Clair", "Monroe", "Jersey", "Clinton"
		))
	) %>%
	select(-c(county_label)) %>% 
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	mutate(
		date = ymd(date), 
		state = relevel(factor(state), ref = "Missouri")
	) %>%
	group_by(county) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(new_cases = (cases - lag(cases, n = lag_period)) / lag_period)

# Testing data
data_testing <- read_csv("data/data-testing.csv") %>% 
	filter(state %in% c("MO", "IL")) %>%
	mutate(state = case_when(
		state == "MO" ~ "Missouri", 
		state == "IL" ~ "Illinois"
	)) %>%
	mutate(state = relevel(factor(state), ref = "Missouri")) %>%
	group_by(state) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		positive_new  = positive - lag(positive, n = lag_period), 
		total_new     = total - lag(total, n = lag_period), 
		rate_positive = positive_new / total_new
	)

# Plot data
date_limits <- c(min(filter(data_cases, cases > 0)$date), max(data_cases$date))
my_theme <- theme(
	text = element_text(size = 9), axis.title.x = element_blank(), 
	plot.caption = element_text(hjust = 0)
)
my_minor_breaks <- c(
	seq(2, 10, by = 2), seq(2, 10, by = 2) * 10, seq(2, 10, by = 2) * 100
)
plot_new_cases <- data_cases %>% 
	filter(cases > 0, !is.na(new_cases), new_cases > 2) %>%
	ggplot(mapping = aes(x = date, y = new_cases, color = county)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.5) + 
		scale_x_date(limits = date_limits, date_labels = "%b %e") + 
		scale_y_log10(
			breaks = 10^c(1:5), minor_breaks = my_minor_breaks
		) + 
		scale_color_hue(l = 50) + 
		labs(
			title = "COVID-19 activity in St Louis metro area", 
			y = paste("New confirmed cases\n(average of past", lag_period, "days)")
		) + 
		my_theme + theme(
			strip.background = element_blank(), 
			# strip.text = element_blank(), 
			legend.position = "top"
		)
plot_testing <- data_testing %>%
	filter(!is.na(rate_positive)) %>% 
	ggplot(mapping = aes(x = date, y = rate_positive)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line(color = grey(0.4)) + geom_point(color = grey(0.4), size = 0.75) + 
		scale_x_date(limits = date_limits, date_labels = "%b %e") + 
		scale_y_continuous(
			limits = c(0, 1), breaks = seq(0, 1, by = 0.5) #, minor_breaks = seq(0, 1, by = 0.2)
		) + 
		labs(
			y = paste("Fraction tests positive\n", "(past", lag_period, "days)"), 
			caption = paste0(
				"\n", "Cases are time-delayed undercount of total infections. ", 
				"Cases are better indicator of virus activity when positive\ntest rate is stable. ", 
				"Data from Johns Hopkins CSSE and The COVID Tracking Project. ", 
				"Plot by jeff smith Ph.D.\nhttps://github.com/matryoshkev/arch-covid"
			)
		) + 
		my_theme + theme(strip.text = element_blank())
pdf(file = NULL)  # start bug workaround
plot_activity <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 19), "null")), 
	list(ggplotGrob(plot_new_cases), ggplotGrob(plot_testing)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 13), b = c(12, 19)
)
dev.off()  # end bug workaround
# dev.new(width = 6, height = 5)
# plot(plot_activity)

ggsave("results/stl-activity.pdf", plot = plot_activity, width = 6, height = 5)
ggsave("results/stl-activity.png", plot = plot_activity, width = 6, height = 5, units = "in")

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
rm(date_limits, my_theme, my_minor_breaks, plot_new_cases, plot_testing, plot_activity)



