# Model of COVID-19 outbreak
# jeff smith 2020-04

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

# Define parameters
total_pop <- 2.8e6 
my_parameters <- c(
	# transmission_rate = 1.1,  # /day
	# transmission_rate = 0.6,  # /day
	# prob_confirmed    = 0.15  # fraction that become confirmed cases
	latency_period    = 3.7,  # days
	infectious_period = 3.5,   # days
	R0 = 2
)
my_parameters <- c(my_parameters, 
	transmission_rate = with(as.list(my_parameters), R0 / infectious_period)  # /day
)
initial_values <- c(S = total_pop - 10, E = 0, I = 100, R = 0)

# Calculate results
my_model <- function(t, initial_values, parameters) {
	with(as.list(c(initial_values, parameters)), {
		dS  <- (- transmission_rate * S * I / total_pop)
		dE  <- (  transmission_rate * S * I / total_pop) - (E / latency_period)
		dI <-  (E / latency_period) - (I / infectious_period)
		dR <-  (I / infectious_period)
		list(c(dS, dE, dI, dR))
	})
}
my_results <- ode(y = initial_values, times = seq(0, 7*21), func = my_model, parms = my_parameters)

# Plot results
# dev.new(width = 5.5, height = 3)
my_plot <- tibble(data.frame(my_results)) %>%
	gather(key = "class", value = "number", -time) %>%
	mutate(
		fraction = number / total_pop, 
		class = case_when(
			class == "S" ~ "Susceptible", 
			class == "E" ~ "Exposed", 
			class == "I" ~ "Infectious", 
			class == "R" ~ "Recovered"
		), 
		class = factor(class, levels = c("Susceptible", "Exposed", "Infectious", "Recovered"))
	) %>%
	ggplot(mapping = aes(x = time, y = fraction, color = class)) + 
		# geom_hline(yintercept = with(as.list(my_parameters), 1/R0)) + 
		geom_line(size = 0.8) + 
		scale_x_continuous(
			breaks = seq(0, 7*21, by = 30)
		) + 
		scale_y_log10(
			limits = c(1e-4, 1), breaks = 10^c(-6:0), minor_breaks = 10^c(-6:0), 
			labels = scales::trans_format("log10", math_format(10^.x))
		) + 
		scale_color_manual(values = c(gray(0.2), "tomato3", "red", gray(0.65))) + 
		labs(x = "time (days)", y = "Fraction of total population") + 
		theme(
			text = element_text(size = 10), legend.title = element_blank()
		)
plot(my_plot)
ggsave("results/SEIR_model.pdf", width = 5.5, height = 3)
