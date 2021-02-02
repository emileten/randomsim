library(data.table)
library(ggplot2)
library(mvtnorm)

ShowSim <- function(n, mu, sigma){

	sim <- rnorm(n, mu, sigma)
	muhat <- mean(sim)
	data <- data.table(x=sim)
	plot <- ggplot(data, aes(x)) + geom_histogram() + geom_vline(xintercept=mu, color='blue') + geom_vline(xintercept=muhat, color='red')
	return(plot)
}


ShowSim(500, 0, 1)
ShowSim(500, 0, 50)


ShowSimMD <- function(n, mu, sigma){
  
  SumMu <- sum(mu) #sum of theoretical mus
  sim <- rmvnorm(n, mu, sigma) #simulation 
  Sumsim <- rowSums(sim) #sum of empirical mus for each element of the simulation
  SumMuhat <- mean(sim)
  data <- data.table(x=Sumsim)
  plot <- ggplot(data, aes(x)) + geom_histogram() + geom_vline(xintercept=SumMu, color='blue') + geom_vline(xintercept=SumMuhat, color='red')
  return(plot)
}

n <- 500
mu <- c(0.5, 0.5)
sigma <- matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2)
sim <- rmvnorm(n, mu, sigma)
sim <- rowSums(sim)
ShowSim(500, 0, 1)
ShowSim(500, 0, 50)
ShowSimMD(500, c(0, 0), matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2))
ShowSimMD(500, c(0, 0), matrix(c(1, 1, 1, 1), 2, 2))

