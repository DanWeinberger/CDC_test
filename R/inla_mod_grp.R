inla_mod_group <- function(select.grp, ds.in){
  
  
  ds <- ds.in[ds.in$age_region_source==select.grp,]
  
  test1 <- inla(all_cause_pre ~ 
                  1 + time_scale + sin1 + sin2 + sin3 + cos1 +cos2+ cos3  , family='poisson', data=ds,
                control.compute=list(config = TRUE))
  
  pred.sample.list <- inla.posterior.sample(n=500, test1, seed=123)
  
  post.labels<-dimnames(pred.sample.list[[1]]$latent)[[1]]
  
  
  
  posterior.samples<- sapply(pred.sample.list, '[[', 'latent') #should be 'joint' instead of 'latent' https://www.ucl.ac.uk/population-health-sciences/sites/population-health-sciences/files/inla_baio.pdf
  
  preds.select<-grep('Predictor',post.labels )
  
  posterior.preds <- (posterior.samples[preds.select,]) #lambda
  
  colnames(posterior.preds) <- paste0('A', 1: ncol(posterior.preds))
  posterior.preds.df <- cbind.data.frame(ds, posterior.preds)

  #Pred by date
  preds.summary <- exp(t(apply(posterior.preds,1, quantile, probs=c(0.025,0.5,0.975))))
  preds.summary <- as.data.frame(preds.summary)
  names(preds.summary) <- c('pred_lcl','pred_mean','pred_ucl')
  preds.summary$age_region_source <- select.grp
  preds.summary$all_cause <- ds$all_cause
  preds.summary$week_end <- ds$week_end
  

  options(dplyr.summarise.inform = FALSE)
  
  posterior.preds.df.qtr <- posterior.preds.df %>%
      group_by(year ,quarter) %>%
      summarise(across(c(all_cause,A1:A500), sum ) ) %>%
      ungroup() %>%
      rowwise() %>%
      mutate( pred_mean= mean(A1:A500), pred_lcl= quantile(A1:A500,probs=0.025),pred_ucl= quantile(A1:A500,probs=0.975), age_region_source=select.grp ) %>%
      ungroup() %>%
      select(year, quarter, pred_mean, pred_lcl, pred_ucl,all_cause ,age_region_source)
    
  posterior.preds.total <- posterior.preds.df[is.na(ds$all_cause_pre),] %>%
    summarise(across(c(all_cause,A1:A500), sum ) ) %>%
    mutate( pred_mean= mean(A1:A500), pred_lcl= quantile(A1:A500,probs=0.025),pred_ucl= quantile(A1:A500,probs=0.975) , age_region_source=select.grp) %>%
    select(pred_mean, pred_lcl, pred_ucl,all_cause,age_region_source )
  
  out.ls = list('preds_qtr'=posterior.preds.df.qtr, 'preds_week'=preds.summary, 'preds_total'=posterior.preds.total )
  return(out.ls)

}
