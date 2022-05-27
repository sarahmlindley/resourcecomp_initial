rm(list = ls())

#Load required libraries
library(ggplot2)
library(deSolve)
library(reshape2)

# Time points
time=seq(from=1,to=100,by=1)

mue <- 0.12
mud <- 0.1
pi  <- 5
b1  <- 0.55
b2  <- 0.45
K1  <- 750
K2  <- 500
r   <- 0.15

parameters <- c(mue, mud, pi, b1, b2)
initial_state_values <- c(E=100,D=100,R=100)

# SIR model function 
si_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
  E = r*E*(1-(E/K1)) - mue
  D = r*D*(1-(D/K2)) - mud
  R = pi - b1*E*R - b2*D*R
    return(list(c(E, D,R)))
  }
  )
}


#Solving the differential equations
output<-as.data.frame(ode(y=initial_state_values,func = si_model,parms=parameters,times = time))
out_long=melt(output,id="time")

plot <- ggplot(data = out_long,          
               aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +xlab("Time (years)")+ylab("Value of the population")+scale_color_discrete(name="State")

