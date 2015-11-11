n_sim = 1e5

n_black = 3
n_drawn = 9
p_black = n_black/n_drawn

# Want posterior for N_total and p_total_black

gen_model = function(prior_N_total,prior_p_total_black)
{
  n_black_bag = rbinom(1, prior_N_total, prior_p_total_black)
  p_black_bag = n_black_bag / prior_N_total
  n_black_hand = rbinom(1,n_drawn,p_black_bag)
  
  return(n_black_hand)
}

priors = data.frame(
  prior_N_total = rpois(n_sim, 30),
  prior_p_total_black = rtruncnorm(n_sim,0,1,0.25,0.1)
)

sims = apply(priors,1, function(x) gen_model(x[1],x[2]))

posterior = priors[sims==3,]

par(mar=c(4,4,4,0.1))
hist(posterior[,1], freq=FALSE, main="Total Jelly Beans")
lines(density( priors[,1] ),col='green',lwd=3)

par(mar=c(4,4,4,0.1))
hist(posterior[,2], freq=FALSE, main="Proportion of Black Jelly Beans")
lines(density( priors[,2] ),col='blue',lwd=3)
