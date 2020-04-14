## Con partes de código de Alison Hill  (https://github.com/alsnhll)

library (deSolve)
library (ggplot2)
library(scales)

W <- 128983681       # susceptibles 13 de abril
X <- 5014          # infectados
Y <- 1964           # removidos
Z <- 9341           # expuestos
N <- W + X + Y + Z

contact_rate <- 2    # contactos por día
transmission_probability <- 0.35       # probabilidad de transmisiíon
infectious_period <- 14                 # periodo infeccioso
latent_period <- 5.5                  # periodo de incubación

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
timepoints <- seq (0, 120, by=1)
output <- lsoda (initial_values, timepoints, seir_model, parameter_list)

Fecha = seq(from = as.Date("2020-04-11"), to = as.Date("2020-08-09"), by = 'day')
Fecha <- as.data.frame(Fecha)
output <- as.data.frame(output)
Fecha$time <- output$time
outputn <- merge(output, Fecha, by = "time")



plot(outputf$I~as.Date(outputf$Fecha,"%d/%m/%y"),type="l", 
     lwd = 4, col = 'blue', ylim = c(0,1),
     ylab = 'Porcentaje de la población',
     main = 'Infectados con SARS-CoV-2 (MX): 15, 7, 3, 2 y 1 contactos al día. Modelo SEIR clásico. @avillalpandoa 13-abr-2020', axes = TRUE)
grid(nx = 18, ny = 6, col = "lightgray", lty = "solid")
par (new = TRUE)

plot(I ~ time, data = outputm, type='l', ylim = c(0,1), col = 'red', lwd = 4, axes = FALSE)
par (new = TRUE)

plot(I ~ time, data = outputr, type='l', ylim = c(0,1), col = 'green', lwd = 4, axes = FALSE)
par (new = TRUE)

plot(I ~ time, data = outputs, type='l', ylim = c(0,1), col = 'purple', lwd = 4, axes = FALSE)
par (new = TRUE)

plot(I ~ time, data = outputn, type='l', ylim = c(0,1), col = 'yellow', lwd = 4, axes = FALSE)
par (new = TRUE)
