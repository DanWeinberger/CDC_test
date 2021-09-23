#Adopted from Zhe Zheng RSV burden analysis
model_string_non_nested<-"
model {
  for (i in 1:n.age){
    for(j in 1:n.state){
      for (t in 1:n.date) { 
    
   log_lambda[i,j,t] <-   beta[1,i,j] +
               beta[2,i,j]*sin1[t] +
               beta[3,i,j]*cos1[t] +
               beta[4,i,j]*sin2[t] +
               beta[5,i,j]*cos2[t] +
               beta[6,i,j]*sin3[t] +
               beta[7,i,j]*cos3[t] +
               beta[8,i,j]*time[t] +
               phi[i,j,t]
               
  y[i,j,t] ~ dpois(exp(log_lambda[i,j,t]))  ## likelihood 
  phi[i,j,t] ~ dnorm(0,tau0)
}

  #The coeeficient for each state/age has some global effect, effect that varies with age, effect that varies with state, and random variation
  for(q in 1:8){ #q for each of the regression effects
    beta[q,i,j] ~ dnorm(0, 1e-4)
  }
  
    }
  }
    
  
  tau0 ~ dgamma(0.01, 0.01)

}
"