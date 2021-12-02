gen_pred_interval_inla <- function(inla_obj, covar.df){
  
  r.samples = inla.posterior.sample(101, inla_obj)
  mod.family <- mod.inla2$.args$family
  
  posterior.preds <- sapply(r.samples,pred.interval.func,dist=mod.family, simplify='array')
  
  posterior.preds.m <- reshape2::melt(posterior.preds)
  posterior.preds.c <- reshape2::dcast(posterior.preds.m, Var1 ~ Var3 + Var2) 
  posterior.preds.c$Var1 <- NULL
  
  colnames(posterior.preds.c) <- paste0('ColA', 1: ncol(posterior.preds.c))
  posterior.preds.df <- cbind.data.frame(covar.df, posterior.preds.c)

  #Pred by date
  preds.summary <- (t(apply(posterior.preds.c,1, quantile, probs=c(0.025,0.5,0.975))))
  preds.summary <- as.data.frame(preds.summary)
  names(preds.summary) <- c('pred_lcl','pred_mean','pred_ucl')
  preds.summary <- cbind.data.frame(preds.summary,covar.df)


  #sex, race_recode, agec
  preds_covars <- cbind.data.frame(posterior.preds.c, covar.df[,c('sex', 'race_recode','agec', 'date','N_deaths')])
  preds_covars.m <- melt(preds_covars, id.vars=c('sex', 'race_recode','agec', 'date','N_deaths'))
  
  out.list= list('preds_time'=preds.summary,'preds_covars.m'=preds_covars.m)
  return(out.list)
}




pred.interval.func <- function(sample.ds,  dist=c('nbinomial','poisson')){
  mu1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])
  if(dist=='nbinomial'){
    nb.size1 = sample.ds$hyperpar
    pred <- replicate(10, rnbinom(n=length(mu1), mu=mu1, size=nb.size1), simplify = 'array')
  }else{
    pred <- replicate(10, rpois(n=length(mu1), lambda=mu1), simplify = 'array')
  }
  return(pred)
}


