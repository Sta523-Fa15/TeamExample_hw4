library(truncnorm)
library(parallel)
library(microbenchmark)


f1 = function(n_sim) 
{
  rtruncnorm(n_sim,0,1,0.25,0.1)
}

f2 = function(n_sim) 
{
  unlist(mclapply(1:10, function (x) rtruncnorm(n_sim/10,0,1,0.25,0.1), mc.cores = 2))
}

f3 = function(n_sim) 
{
  unlist(mclapply(1:10, function (x) rtruncnorm(n_sim/10,0,1,0.25,0.1), mc.cores = 4))
}     

f4 = function(n_sim) 
{
  unlist(mclapply(1:10, function (x) rtruncnorm(n_sim/10,0,1,0.25,0.1), mc.cores = 8))
}     

m = microbenchmark(f1(1e5),f2(1e5),f3(1e5),f4(1e5),times = 10)



run_abc = function(n_sim = 1e5)
{
  n_black = 3
  n_drawn = 9
  p_black = n_black/n_drawn
  
  # Want posterior for N_total and p_total_black
  
  prior_N_total = rpois(n_sim, 30)
  if (n_sim < 1e6)
    prior_p_total_black = rtruncnorm(n_sim,0,1,0.25,0.1)
  else
    prior_p_total_black = f4(n_sim)
  
  n_black_bag = rbinom(n_sim, prior_N_total, prior_p_total_black)
  p_black_bag = n_black_bag / prior_N_total
    
  n_black_hand = rbinom(n_sim,n_drawn,p_black_bag)

  
  posterior_N = prior_N_total[n_black_hand==3]
  posterior_prop = prior_p_total_black[n_black_hand==3]
  
  par(mar=c(4,4,4,0.1))
  hist(posterior_N, freq=FALSE, main="Total Jelly Beans")
  lines(density( prior_N_total ),col='green',lwd=3)
  
  par(mar=c(4,4,4,0.1))
  hist(posterior_prop, freq=FALSE, main="Proportion of Black Jelly Beans")
  lines(density( prior_p_total_black ),col='blue',lwd=3)
}