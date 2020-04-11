# Epidemiological dynamics of COVID-19 in the St Louis metropolitan area
# jeff smith 2020

# Plot data
# - cumulative cases vs time
# - new cases (in last X days) vs time
# 
# - map current cases by zip
# - cumulative deaths
# - new deaths
# 

# Dependencies
library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("gtable")

#
# Load and process data
#

data_counties <- read_csv("data/county_stl.csv") %>%
	rename(cases = confirmed) %>%
	mutate(date = ymd(report_date)) %>%
	select(date, cases, county) %>%
	filter(
		(date >= "2020-03-02") & 
		county %in% c(
			"St. Louis City", "St. Louis", "St. Charles", "Jefferson", 
			"Madison", "St. Clair", "Monroe", 
			"Lincoln", "Warren", "Franklin", "Clinton", "Jersey"
		)
	) %>%
	mutate(county = ifelse(!grepl("City", county), paste(county, "County"), county))
data_metro <- data_counties %>%
	group_by(date) %>%
	summarise(cases = sum(cases, na.rm = TRUE)) %>%
	mutate(county = "St. Louis Metro")

# Calculate new cases
serial_interval = 5
data_counties <- data_counties %>%
	group_by(county) %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		cases_new  = cases - lag(cases, n = 1), 
		cases_prev = cases - lag(cases, n = serial_interval), 
		cases_next = lead(cases, n = serial_interval) - cases
	) %>%
	mutate(R_effective = cases_next / cases_prev) %>%
	ungroup()
data_metro <- data_metro %>%
	arrange(date, .by_group = TRUE) %>%
	mutate(
		cases_new  = cases - lag(cases, n = 1), 
		cases_prev = cases - lag(cases, n = serial_interval), 
		cases_next = lead(cases, n = serial_interval) - cases
	)


# tail(data_counties)

# Latest R_effective
R_effective_last <- data_counties %>%
	filter(date == max(date) - serial_interval) %>%
	select(county, R_effective)
tmp <- R_effective_last$R_effective
names(tmp) <- R_effective_last$county
R_effective_last <- tmp
Find_R_effective_last <- function(x){ return(R_effective_last[[x]]) }
data_counties$R_effective_last <- sapply(data_counties$county, Find_R_effective_last)
rm(R_effective_last)




# 
# Plot: Total and new cases for total St Louis metro area
# 

plot_metro_total <- ggplot(data_metro) + 
	aes(x = date, y = cases) + 
	geom_col() + 
	scale_x_date(limits = c(ymd("2020-03-01"), max(data_metro$date) + 1)) + 
	labs(
		title = "Spread of COVID-19 in the St. Louis metro area", 
		y = "Total confirmed cases"
	) + 
	theme(text = element_text(size = 10), axis.title.x = element_blank())
plot_metro_new <- ggplot(subset(data_metro, !is.na(cases_prev))) + 
	aes(x = date, y = cases_prev/serial_interval) + 
	geom_col() + 
	scale_x_date(limits = c(ymd("2020-03-01"), max(data_metro$date) + 1)) + 
	labs(
		y = paste("New confirmed cases\n(average of last ", serial_interval, " days)", sep = ""), 
		caption = (paste(
			"\n Plot by jeff smith, Ph.D. ", 
			"with data compiled by Chris Prener, Ph.D.\n", 
			"from Johns Hopkins University CSSE and New York Times COVID-19 Projects\n", 
			"Latest data: ", max(data_metro$date), 
			sep = ""
		))
	) + 
	theme(text = element_text(size = 10), axis.title.x = element_blank())
pdf(file = NULL)  # start workaround
plot_metro_combined <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 13), "null")), 
	list(ggplotGrob(plot_metro_total), ggplotGrob(plot_metro_new)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 7), b = c(6, 13)
)
dev.off()  # end workaround
dev.new(width = 5, height = 5)
plot(plot_metro_combined)
ggsave("results/metro.pdf", plot_metro_combined, device = "pdf", width = 5, height = 5)

dev.new(width = 5, height = 4)
# ggplot(data = subset(data_metro, cases > 0)) + 
	# aes(x = date, y = cases) + 
	# geom_point() + geom_line() + 
	# scale_y_log10(limits = c(1, 1e4), minor_breaks = 10^c(1:6)) + 
	# labs(y = "Total confirmed cases", x = "Date")



# 
# Plot: Total and new cases by county
# 

# dev.new(width = 6.5, height = 3)
# ggplot(subset(data_counties, !is.na(cases_new))) + 
	# aes(x = date, y = cases_new, fill = county) + 
	# geom_col() + 
	# scale_x_date(limits = c(ymd("2020-03-01"), max(data_metro$date) + 1)) + 
	# theme(text = element_text(size = 10), axis.title.x = element_blank())

data_for_plot <- data_counties %>%
	mutate(my_facet = county %in% c(
		# "St. Louis City", 
		# "St. Charles County", 
		# "Jefferson County"
		# "Franklin County", 
		# "Warren County" 
		"Jersey County", 
		"Madison County", 
		"St. Clair County", 
		"Monroe County", 
		"Clinton County"
	))
plot_counties_total <- ggplot(subset(data_for_plot, cases > 0)) + 
	aes(x = date, y = cases, group = county, color = county) + 
	facet_wrap(~ my_facet, ncol = 2) + 
	geom_point(size = 0.5) + geom_line(alpha = 0.75) + 
	geom_point(
		mapping = aes(group = county, fill = county), 
		data = filter(data_for_plot, date == max(data_for_plot$date)), 
		size = 1.5
	) + 
	geom_text(
		mapping = aes(label = county), 
		data = subset(data_for_plot, date == max(data_for_plot$date)), 
		hjust = 0, size = 2.5, nudge_x = 1
	) + 
	scale_x_date(limits = c(ymd("2020-03-07"), max(data_for_plot$date + 12))) + 
	scale_y_log10(limits = c(1, 1e4), minor_breaks = 10^c(1:6)) + 
	scale_color_hue(l = 50) + scale_fill_hue(l = 50) + 
	labs(
		y = "Total confirmed cases", 
		title = "Spread of COVID-10 in the St. Louis metro area"
	) + 
	theme(
		text = element_text(size = 10), axis.title.x = element_blank(), 
		strip.text = element_blank(), strip.background = element_blank(), 
		legend.position = "none"
	)
plot_counties_new <- ggplot(subset(data_for_plot, !is.na(cases_prev) & (cases_prev/serial_interval >= 1) )) + 
	aes(x = date, y = cases_prev/serial_interval, group = county, color = county) + 
	facet_wrap(~ my_facet, ncol = 2) + 
	geom_point(size = 0.5) + geom_line(alpha = 0.75) + 
	geom_point(
		mapping = aes(group = county, fill = county), 
		data = filter(data_for_plot, date == max(data_for_plot$date)), 
		size = 1.5
	) + 
	geom_text(
		mapping = aes(label = county), 
		data = filter(data_for_plot, date == max(data_for_plot$date)), 
		hjust = 0, size = 2.5, nudge_x = 1
	) + 
	scale_x_date(limits = c(ymd("2020-03-07"), max(data_for_plot$date + 12))) + 
	scale_y_log10(limits = c(1, 1e3), minor_breaks = 10^c(1:6)) + 
	scale_color_hue(l = 50) + 
	labs(
		y = paste("New confirmed cases\n(average of last ", serial_interval, " days)", sep = ""), 
		caption = (paste(
			"\n Plot by jeff smith, Ph.D. ", 
			"with data compiled by Chris Prener, Ph.D.\n", 
			"from Johns Hopkins University CSSE and New York Times COVID-19 Projects\n", 
			"Latest data: ", max(data_for_plot$date), 
			sep = ""
		))
	) + 
	theme(
		text = element_text(size = 10), axis.title.x = element_blank(), 
		strip.text = element_blank(), strip.background = element_blank(), 
		legend.position = "none"
	)
pdf(file = NULL)  # start workaround
plot_counties_combined <- gtable_add_grob(
	gtable(widths = unit(rep(1, 1), "null"), heights = unit(rep(1, 13), "null")), 
	list(ggplotGrob(plot_counties_total), ggplotGrob(plot_counties_new)), 
	l = c(1, 1), r = c(1, 1), t = c(1, 7), b = c(6, 13)
)
dev.off()  # end workaround
# dev.new(width = 6.5, height = 6)
# plot(plot_counties_combined)
ggsave("results/counties.pdf", plot_counties_combined, device = "pdf", width = 6.5, height = 6)


# 
# Plot: New cases vs previous cases (log-log)
# 

# Clean dataset for log-scale plotting
# data_counties_log <- data_counties %>% 
	# filter(
		# !is.na(cases_next) & !is.na(cases_prev) & 
		# (cases_next > case_cutoff) & (cases_prev > case_cutoff)
	# ) %>% 
	# mutate(county = factor(county))

case_cutoff <- 5
data_for_plot <- data_counties %>%
	filter(
		!is.na(cases_next) & !is.na(cases_prev) & 
		(cases_next > case_cutoff) & (cases_prev > case_cutoff)
	) %>% 
	mutate(my_facet = county %in% c(
		"St. Charles County", 
		"St. Clair County", 
		"Franklin County" 
		# "Jefferson County" 
	))
my_limits <- c(5, 2e3)
my_minor_breaks <- 10^c(1:6)
plot_stepwise <- ggplot(data = data_for_plot) + 
	aes(x = cases_prev, y = cases_next, color = R_effective_last) + 
	facet_wrap(~ my_facet, ncol = 2) + 
	geom_line(
		data = tibble(cases_prev = my_limits, cases_next = my_limits), 
		color = grey(0.75), linetype = "dashed"
	) + 
	# geom_line(
		# data = tibble(cases_prev = c(5, 1e3), cases_next = c(10, 2e3)), 
		# color = grey(0.75), linetype = "dotted"
	# ) + 
	annotate(
		"text", label = "Effective R = 1", 
		size = 2.5, color = grey(0.5), angle = 39,
		x = my_limits[2], y = my_limits[2], hjust = 1, vjust = -0.5
	) + 
	geom_path(aes(group = county), alpha = 0.75) + 
	geom_point(aes(group = county), size = 0.5) + 
	geom_point(
		mapping = aes(group = county, fill = R_effective_last), 
		data = filter(data_for_plot, date == max(data_for_plot$date)), 
		size = 2
	) + 
	geom_text(
		mapping = aes(label = county), 
		data = filter(data_for_plot, date == max(data_for_plot$date)), 
		hjust = -0.13, size = 2.5
	) + 
	scale_x_log10(limits = my_limits, minor_breaks = my_minor_breaks) + 
	scale_y_log10(limits = my_limits, minor_breaks = my_minor_breaks) + 
	scale_color_gradient(low = grey(0.3), high = "red") + 
	scale_fill_gradient(low = grey(0.3), high = "red") + 
	labs(
		title = "Spread of COVID-19 in the St. Louis metro area", 
		x = paste("Cases confirmed in previous", serial_interval, "days"), 
		y = paste("Cases confirmed in next", serial_interval, "days"), 
		caption = (paste(
			"\n",
			"Plot by jeff smith, Ph.D. ", 
			"with data compiled by Chris Prener, Ph.D.\n", 
			"from Johns Hopkins University CSSE and New York Times COVID-19 Projects\n", 
			"Showing data >", case_cutoff, " cases. ", 
			"Large points show counties on ", max(data_counties$date) - serial_interval, 
			sep = ""
		))
	) + 
	theme(
		strip.text = element_blank(), strip.background = element_blank(), 
		legend.position = "none", text = element_text(size = 10)
	)
# dev.new(width = 6.5, height = 3.5)
# plot(plot_stepwise)
ggsave("results/stepwise.pdf", plot_stepwise, device = "pdf", width = 6.5, height = 3.5)

# Clean up
rm(serial_interval, my_limits, my_minor_breaks, data_for_plot)
