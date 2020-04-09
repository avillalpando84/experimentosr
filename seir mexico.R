## Con partes de código de Alison Hill  (https://github.com/alsnhll)

library (deSolve)
W <- 129000000        # susceptibles 8 de abril
X <- 3182           # infectados
Y <- 633           # removidos
Z <- 9188           # expuestos
N <- W + X + Y + Z

contact_rate <- 5    # contactos por día
transmission_probability <- 0.35       # probabilidad de transmisiíon
infectious_period <- 14                 # periodo infeccioso
latent_period <- 6.4                  # periodo de incubación
beta_value <- contact_rate * transmission_probability
gamma_value <- 1 / infectious_period
delta_value <- 1 / latent_period
Ro <- beta_value / gamma_value
parameter_list <-  c (beta = beta_value, gamma = gamma_value, delta = delta_value)

seir_model <- function (current_timepoint, state_values, parameters)
{
  # variables de estado
  S <- state_values [1]        # susceptibles
  E <- state_values [2]        # expuestos
  I <- state_values [3]        # infecciosos
  R <- state_values [4]        # removidos
  with ( 
    as.list (parameters), 
    {
      # derivadas
      dS <- (-beta * S * I)
      dE <- (beta * S * I) - (delta * E)
      dI <- (delta * E) - (gamma * I)
      dR <- (gamma * I)
      
      results <- c (dS, dE, dI, dR)
      list (results)
    }
  )
}
  
initial_values <- c (S = W/N, E = X/N, I = Y/N, R = Z/N)
timepoints <- seq (0, 90, by=1)
output <- lsoda (initial_values, timepoints, seir_model, parameter_list)

plot (S ~ time, data = output, type='l', col = 'blue') 
plot (E ~ time, data = output, type='l', col = 'pink')  
plot (I ~ time, data = output, type='l', col = 'red') 
plot (R ~ time, data = output, type='l', col = 'green')  
  
  # susceptibles
plot (S ~ time, data = output, type='l', ylim = c(0,1), lwd = 4, xlab = "Tiempo en días", col = 'blue', ylab = 'Porcentaje de la población', main = 'Estimación de personas susceptibles, expuestas, infectadas y recuperadas') 
par (new = TRUE)    
  
  # expuestos
plot (E ~ time, data = output, type='l', ylim = c(0,1), lwd = 4, col = 'pink', ylab = '', axes = FALSE)
par (new = TRUE) 
  
  # infecciosos
plot (I ~ time, data = output, type='l', ylim = c(0,1), lwd = 4, col = 'red', ylab = '', axes = FALSE) 
par (new = TRUE)  
  
  # removidos
plot (R ~ time, data = output, type='l', ylim = c(0,1), lwd = 4, col = 'green', ylab = '', axes = FALSE)