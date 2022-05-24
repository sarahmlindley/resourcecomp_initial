#Load required libraries
library(ggplot2)
library(deSolve)
library(reshape2)

# Forage functions
dforage <- function(R){
  fe <- 0.25/R
  return(fe)
}
eforage <- function(R){
  fe <- 0.25/R
  return(fe)
}

# Model inputs
initial_state_values=c(Se=1000,Sd=1000, R=1000)

musd  <- 0.25    #mortality of sus deer
muid  <- 0.77   #mortality of inf deer
muse  <- 0.25   #mortality of sus elk
muie  <- 0.82   #mortality of inf elk
delta <- 0.1    #rate of prion excretion
mup   <- 0.1    #rate of prion decay
ud    <- 0.787  #environ transmission, deer
ue    <- 0.5    #environ transmission, elk
gamma <- 0.25   #inter-species trans rate
z     <- 0.3    #percent prions digested
phie  <- 0.4    #fecundity rdxn, elk
phid  <- 0.4    #fecundity rdxn, deer
ee    <- 1     #conversion of resource to births, elk
ed    <- 1     #conversion of resource to births, deer
#pi    <- 100    #fixed resource
mur   <- 0.01   #decay of resources

parameters <- c(musd, muid, muse, muie, delta, mup, ud, ue, gamma,
                z, phie, phid, ee, ed, pi)

# Time points
time=seq(from=1,to=100,by=1)

# SIR model function 
si_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    Se = ee * eforage(R)*R*(Se) - muse*Se  
    #Ie = ue*Se*P - muie*Ie - he + gamma*Se*Id
    Sd = ed * dforage(R)*R*(Sd) - musd*Sd 
    #Id = ud*Sd*P - muid*Id - hd + gamma*Sd*Ie
    #P  = delta*Ie + delta*Id - mup*P - z*eforage(R)*(Se + Ie)*P - z*dforage(R)*(Sd+Id)*P
    R  = -mur*R - eforage(R)*(Se)*R - dforage(R)*(Sd)*R
    return(list(c(Se, Sd, R)))
  }
  )
}


#Solving the differential equations
output<-as.data.frame(ode(y=initial_state_values,func = si_model,parms=parameters,times = time))

out_long=melt(output,id="time")
elk <- subset(out_long,variable!="Sd"&variable!="Id")
deer <- subset(out_long,variable!="Se"&variable!="Ie")
# To plot the proportion of susceptible, infected and recovered individuals over time


plot <- ggplot(data = out_long,          
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +xlab("Time (days)")+ylab("Value of the population")+scale_color_discrete(name="State")+xlim(0,100)


