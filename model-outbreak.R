# Model of COVID-19 outbreak
# jeff smith 2020-04

# TO DO
# - proper humped distributions for incubation and infectious periods

# Dependencies
library("deSolve")
library("dplyr")
library("tidyr")
library("ggplot2")
library("scales")

# 
# SEIR model 
# 

# S = Susceptible
# E = Exposed
# I = Infectious
# R = Recovered

# Define parameters
my_parameters <- c(
	incubation_time   = 3.7,  # days
	infectious_period = 5,   # days
	R0 = 3
)
my_parameters <- c(my_parameters, 
	transmission_rate = with(as.list(my_parameters), R0 / infectious_period)  # /day
)
total_pop <- 2.8e6
initial_values <- c(S = total_pop - 10, E = 0, I = 10, R = 0)

# Calculate results
my_model <- function(t, initial_values, parameters) {
	with(as.list(c(initial_values, parameters)), {
		dS  <- (- transmission_rate * S * I / total_pop)
		dE  <- (  transmission_rate * S * I / total_pop) - (E / incubation_time)
		dI <-  (E / incubation_time) - (I / infectious_period)
		dR <-  (I / infectious_period)
		list(c(dS, dE, dI, dR))
	})
}
my_results <- ode(y = initial_values, times = seq(0, 7*21), func = my_model, parms = my_parameters)

# Format results
my_results <- tibble(data.frame(my_results)) %>% 
	mutate(I_tot = E + I) %>%
	gather(key = "class", value = "number", -time) %>%
	mutate(
		fraction = number / total_pop, 
		class = case_when(
			class == "S"     ~ "Susceptible", 
			class == "E"     ~ "Exposed", 
			class == "I"     ~ "Infectious", 
			class == "I_tot" ~ "Infected", 
			class == "R"     ~ "Recovered"
		), 
		class = factor(class, levels = c("Susceptible", "Exposed", "Infectious", "Infected", "Recovered"))
	)

# Plot results
if (dev.cur() < 2) { dev.new(width = 5.5, height = 3) }

my_plot <- my_results %>% 
	filter(class != "Infected") %>% 
	ggplot(mapping = aes(x = time, y = fraction, color = class)) + 
		geom_hline(
			yintercept = with(as.list(my_parameters), 1/R0), 
			color = gray(0.6), linetype = "dashed"
		) + 
		annotate(
			geom = "text", label = as.expression(bquote(1/R[0])), 
			x = 0, y = with(as.list(my_parameters), 1/R0), 
			hjust = 0, vjust = 1.5, color = gray(0.6), size = 3
		) + 
		geom_line(size = 0.8) + 
		scale_x_continuous(
			limits = c(0, 120), 
			breaks = seq(0, 7*21, by = 30)
		) + 
		scale_y_log10(
			limits = c(1e-5, 1), breaks = 10^c(-6:0), minor_breaks = 10^c(-6:0), 
			labels = scales::trans_format("log10", math_format(10^.x))
		) + 
		scale_color_manual(values = c(gray(0.2), "tomato3", "red", gray(0.65))) + 
		labs(x = "time (days)", y = "Fraction of total population") + 
		theme(text = element_text(size = 10), legend.title = element_blank())
plot(my_plot)

ggsave("results/SEIR_model.pdf", width = 5.5, height = 3)

# Plot with exposed and infectious lumped together
my_results %>% 
	filter( !(class %in% c("Exposed", "Infectious")) ) %>% 
	ggplot(mapping = aes(x = time, y = fraction, color = class)) + 
		geom_hline(
			yintercept = with(as.list(my_parameters), 1/R0), 
			color = gray(0.6), linetype = "dashed"
		) + 
		annotate(
			geom = "text", label = as.expression(bquote(1/R[0])), 
			x = 0, y = with(as.list(my_parameters), 1/R0), 
			hjust = 0, vjust = 1.5, color = gray(0.6), size = 3
		) + 
		geom_line(size = 0.8) + 
		scale_x_continuous(breaks = seq(0, 7*21, by = 30)) + 
		scale_y_log10(
			limits = c(1e-4, 1), breaks = 10^c(-6:0), minor_breaks = 10^c(-6:0), 
			labels = scales::trans_format("log10", math_format(10^.x))
		) + 
		scale_color_manual(values = c(gray(0.2), "red", gray(0.65))) + 
		labs(x = "time (days)", y = "Fraction of total population") + 
		theme(text = element_text(size = 10), legend.title = element_blank())

