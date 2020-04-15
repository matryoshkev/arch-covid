# Model of COVID-19 outbreak
# jeff smith 2020-04


library("deSolve")
library("tidyr")
library("ggplot2")

# 
# SEIR model 
# 

# S = Susceptible
# E = Exposed
# I = Infectious

total_pop = 2.8e6 
my_parameters <- c(
	# transmission_rate = 1.1,  # /day
	transmission_rate = 0.6,  # /day
	latency_period    = 3.7,  # days
	infectious_period = 3.5,  # days
	prob_confirmed    = 0.15  # fraction that become confirmed cases
)
initial_values <- c(S = total_pop - 10, E = 0, I = 100)
my_model <- function(t, initial_values, parameters) {
	with(as.list(c(initial_values, parameters)), {
		dS  <- (- transmission_rate * S * I / total_pop)
		dE  <- (  transmission_rate * S * I / total_pop) - (E / latency_period)
		dI <-  (E / latency_period) - (I / infectious_period)
		list(c(dS, dE, dI))
	})
}
times <- c(0:150)


cat(
	"Effective reproductive number R =", 
	with(as.list(my_parameters), transmission_rate * infectious_period)
)

my_results <- ode(y = initial_values, times = times, func = my_model, parms = my_parameters)

data_for_plot <- tibble(data.frame(my_results))
data_for_plot <- gather(data_for_plot, key = "class", value = "number", -time)
ggplot(data_for_plot) + 
	aes(x = time, y = number, color = class) + 
	geom_line() + 
	scale_y_log10(limits = c(10, total_pop))
	# scale_y_continuous(limits = c(0, total_pop))


