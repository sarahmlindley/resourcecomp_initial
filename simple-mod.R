rm(list = ls())

#Load required libraries
library(ggplot2)
library(deSolve)
library(reshape2)

# Time points
time=seq(from=1,to=25,by=1)

mue <- 0.25
mud <- 0.2
pi  <- 1
b1  <- 1
b2  <- 0.5

parameters <- c(mue, mud, pi, b1, b2)
initial_state_values <- c(E=1,D=1,R=9)

# SIR model function 
si_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
  E = 0.1*E*R - mue*E
  D = 0.1*D*R - mud*D
  R = pi*(10-R) - b1*E*R - b2*D*R
    return(list(c(E, D, R)))
  }
  )
}


#Solving the differential equations
output<-as.data.frame(ode(y=initial_state_values,func = si_model,parms=parameters,times = time))
out_long=melt(output,id="time")

plot <- ggplot(data = out_long,          
               aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +xlab("Time (years)")+ylab("Value of the population")+scale_color_discrete(name="State")

