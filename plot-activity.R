# Plot SARS-CoV-2 activity in St Louis metro area
# by county

# Dependencies
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("gtable")

lag_time <- 5 


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
	select(-c(UID, county_label)) %>% 
	pivot_longer(cols = contains("-"), names_to = "date", values_to = "cases") %>%
	mutate(date = ymd(date)) %>%
	# filter(
		# ((state == "Missouri") & (date >= ymd("2020-03-28"))) | 
		# ((state == "Illinois") & (date >= ymd("2020-03-29")))
	# ) %>%
	mutate(state = relevel(factor(state), ref = "Missouri")) %>%
	group_by(county) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		new_cases = (cases - lag(cases, n = lag_time)) / lag_time, 
		# rate_increase = log(cases / lag(cases, n = lag_time)) / lag_time
	)

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
		positive_new  = positive - lag(positive, n = lag_time), 
		total_new     = total - lag(total, n = lag_time), 
		rate_positive = positive_new / total_new
	)




# Plot data

date_limits <- c(min(filter(data_cases, cases > 0)$date), max(data_cases$date))
my_theme <- theme(text = element_text(size = 9), axis.title.x = element_blank())


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

# Plot new case activity
# dev.new(width = 5.5, height = 3)
plot_new_cases <- data_cases %>% 
	filter(cases > 0, !is.na(new_cases), new_cases > 3) %>%
	ggplot(mapping = aes(x = date, y = new_cases, color = county)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.5) + 
		scale_x_date(limits = date_limits) + 
		scale_y_log10() + 
		labs(
			title = "COVID-19 activity in St Louis metro area", 
			y = paste("New confirmed cases\n(average of past", lag_time, "days)")
		) + 
		my_theme + 
		theme(
			# strip.background = element_blank(), 
			# strip.text = element_blank(), 
			legend.position = "top"
		)
# plot(plot_new_cases)

# Plot positive test rate
plot_positive <- data_testing %>%
	filter(!is.na(rate_positive)) %>% 
	ggplot(mapping = aes(x = date, y = rate_positive)) + 
		facet_wrap(~ state, ncol = 2) + 
		geom_line() + geom_point(size = 0.75) + 
		scale_x_date(limits = date_limits) + 
		scale_y_continuous(
			limits = c(0, 1), 
			breaks = seq(0, 1, by = 0.5), 
			minor_breaks = seq(0, 1, by = 0.1)
		) + 
		labs(
			y = paste(
				"Positive test rate\n", "(past", lag_time, "days)"
			), 
			caption = paste0(
				"\n", 
				"Cases are better indicator of virus activity when positive test rate is stable\n", 
				"Cases are time-delayed undercount of actual infections\n", 
				"Data from Johns Hopkins CSSE and The COVID Tracking Project\n"
			)
		) + 
		my_theme + 
		theme(strip.text = element_blank())
# plot(plot_positive)

# Make combined plot
pdf(file = NULL)  # start bug workaround
plot_activity <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 17), "null")), 
	list(ggplotGrob(plot_new_cases), ggplotGrob(plot_positive)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 11), b = c(10, 17)
)
dev.off()  # end bug workaround
# dev.new(width = 6, height = 5.5)
# plot(plot_activity)
ggsave("results/stl-activity.pdf", plot = plot_activity, width = 6, height = 5)



# data_cases %>% 
	# filter(cases > 0, !is.na(rate_increase)) %>%
	# ggplot(mapping = aes(x = date, y = rate_increase, color = county)) + 
		# facet_wrap(~ state, ncol = 2) + 
		# geom_line() + geom_point(size = 0.5) + 
		# my_theme


plot_cases <- data_metro %>%
	ggplot(mapping = aes(x = date, y = cases)) + 
	geom_col() + 
	labs(y = "Cumulative confirmed cases") + 
	my_theme
plot_new_cases <- data_metro %>%
	ggplot(mapping = aes(x = date, y = new_cases)) + 
	geom_col() + 
	labs(
		y = "New confirmed cases", 
		caption = (paste0("\n Plot by jeff smith. Data from Johns Hopkins University CSSE")) 
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

# Clean up
rm(data_metro, lag_time)
rm(my_theme, plot_cases, plot_new_cases, plot_metro)



