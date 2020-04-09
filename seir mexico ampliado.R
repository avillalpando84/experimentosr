## Modelo construido a partir de "SEIR Transmission dynamics model of 2019 nCoV coronavirus with considering
## the weak infectious ability and changes in latency duration" de Shi Pengpeng, Cao Shengli y Feng Peihua.
## Con partes de código de Alison Hill  (https://github.com/alsnhll)

## 8 de abril de 2019
library (deSolve)
library(ggplot2)
library(scales)
## VARIABLES
S <-  129000000 # población
E <-  9188 # expuestos
I <-  3181 # infectados 8 de abril MÉXICO
Sq <- 38700000 #población en cuarentena
Eq <- 1287 #Población expuesta en cuarentena
H <-  I + Eq
R <-  633 #recuperados

## PARÁMETROS
c <- 3    # contactos por día
beta <- 0.35       # probabilidad de transmisiíon
q <- 0.09           #radio de cuarentena
theta <- 1          #probabilidad de contagio en latencia
lambda <- 1/14         #duración de la cuarentena
delta_i <- 0.03 #cuantos enfermos son cuarentenados
delta_q <- 0.14 #tasa de transformación de expuestos a aislados infectados
gama_i <- 0.003 #tasa de recuperación de los infectados
gama_h <- 0.009 #tasa de recuperación de los aislados infectados
alfa <- 0.05 # "desease-induced death date 
sigma <- 1/7 #tiempo de incubación

parameter_list <-  c (c, beta, q, theta, lambda, delta_i, delta_q, gama_i, gama_h, alfa, sigma)
seirplus_model <- function (current_timepoint, state_values, parameters)
{
  # variables de estado
  S <- state_values  [1]        # susceptibles
  E <- state_values  [2]        # expuestos
  I <- state_values  [3]        # infecciosos
  Sq <- state_values [4]        # susceptibles aislados
  Eq <- state_values [5]        # expuestos aislados 
  H <- state_values  [6]        # hospitalizados
  R <- state_values  [7]        # removidos
  with ( 
    as.list (parameters), 
    {
      # derivadas
      dS <- -(beta*c+c*q*(1-beta))*S*(I+theta*E)+lambda*Sq
      dE <- beta*c*(1-q)*S*(I+theta*E)-sigma*E
      dI <- sigma*E-(delta_i+alfa+gama_i)*I
      dSq <- (1-beta)*c*q*S*(I+theta*E)-lambda*Sq
      dEq <- beta*c*q*S*(I+theta*E)-delta_q*Eq
      dH <- delta_i*I+delta_q*Eq-(alfa+gama_h)*H  
      dR <- gama_i*I+gama_h*H
      
      results <- c (dS, dE, dI, dSq, dEq, dH, dR)
      list (results)
    }
  )
}

initial_values <- c (S, E, I, Sq, Eq, H, R)
timepoints <- seq (0, 60, by=1)
output <- lsoda (initial_values, timepoints, seirplus_model, parameter_list)
output <- as.data.frame(output)

names(output)[2] <- "S"
names(output)[3] <- "E"
names(output)[4] <- "I"
names(output)[5] <- "Sq"
names(output)[6] <- "Eq"
names(output)[7] <- "H"
names(output)[8] <- "R"

Fecha = seq(from = as.Date("2020-04-08"), to = as.Date("2020-06-07"), by = 'day')
Fecha <- as.data.frame(Fecha)
Fecha$time <- (output$time)
outputf <- merge(output, Fecha, by = "time")

grafIH <- ggplot() + geom_line(data=outputf, aes(x=Fecha, y=I), linetype="solid", color="blue", size=2) +
             geom_line(data=outputf, aes(x=Fecha, y=H), linetype="solid", color="red", size=2) + labs(x = "Fecha") +
             labs(title = "Contagios y hospitalizaciones COVID-19 México", subtitle = "Contagiados en azul, hospitalizaciones necesarias en rojo") + 
             labs(y = "Habitantes") + scale_y_continuous(labels = comma)

grafIH


