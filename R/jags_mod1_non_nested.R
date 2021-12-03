#Adopted from Zhe Zheng RSV burden analysis
model_string_non_nested<-"
model {
  for (i in 1:n.age){
    for(j in 1:n.race){
      for(k in 1:2){
      for (t in 1:n.date) { 
    
   log_lambda[i,j,k,t] <-   beta[1,i,j] +
               beta[2,i,j]*qtr1[t] +
               beta[3,i,j]*qtr2[t] +
               beta[4,i,j]*qtr3[t] +
               beta[5,i,j]*time[t] +
               phi[i,j,k,t]
               
  y[i,j,k,t] ~ dpois(exp(log_lambda[i,j,k,t]))  ## likelihood 
  phi[i,j,t] ~ dnorm(0,tau0)
}

  #The coeeficient for each state/age has some global effect, effect that varies with age, effect that varies with state, and random variation
  for(q in 1:5){ #q for each of the regression effects
    beta[q,i,j] <- alpha0[q] + alpha_race[q,j] + alpha_agec[q,i] +  alpha_sex[q,k] + phi2[q,i,j]
    
  }
  
    }
    }
  
  
    
    for (i in 1:n.age){
      for(q in 1:5){
       alpha_agec[q,i] ~ dnorm(mu_agec, tau_agec)
      }
    }
    
  tau0 ~ dgamma(0.01, 0.01)
  alpha0 ~ dnorm(0,1e-4)
  mu_agec ~dnorm(0, 1e-4)

}
"