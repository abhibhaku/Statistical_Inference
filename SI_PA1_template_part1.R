

set.seed(5) # setting seed value
lambda <- 0.2 # lambda value specified
nos <- 40 # number of samples
sim <- 1000 # no. of simulations specified

# Q1 - Show the sample mean and compare it to the theoretical mean of the distribution

sim_exp <- replicate(sim,rexp(nos,lambda)) # running simulations

mean_exp <- apply(sim_exp,2,mean) # calculating mean for each sample

s_mean <- mean(mean_exp) # calculated mean from the sample
s_mean
t_mean <- 1/lambda # calculated theoretical mean
t_mean

hist(mean_exp, col="white", main = "Exponential Function Simulation - Means",breaks=40, xlim = c(3,8)) # plotting histogram for means
abline(v=s_mean, lwd = "4", col = "red")
abline(v=t_mean, lwd = "4", col = "blue")

# Result for Q1 - We can clearly see from above results & plot, that sample mean and theoretical mean are very close to each other.

# Q2 - Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution

s_var <- var(mean_exp) # calculating sample variance
s_var
t_var <- ((1/lambda)/sqrt(nos))^2 # calculating theoretical variance
t_var

# Result for Q2 - We can clearly see from above results, that sample variance and theoretical variance are very close to each other.

# Q3 - Show that the distribution is approximately normal

hist(mean_exp, prob=TRUE,col="white", main = "Exponential Function Simulation - Means",breaks=40, xlim = c(3,8)) # plotting histogram for means
lines(density(mean_exp),lwd=3,col="blue") # plotting the distribution curve
# plotting the normal distribution curve
a <- seq(min(mean_exp), max(mean_exp), length=2*nos)
b <- dnorm(a, mean=1/lambda, sd=sqrt(((1/lambda)/sqrt(nos))^2))
lines(a, b, pch=22, col="red", lwd=2, lty = 2)

# comparing the quantile plots

qqnorm(mean_exp)
qqline(mean_exp, col = 2)

# Result for Q3 - From the above two plots, we can safely conclude that the distribution of mean of 40 exponentials is very close to normal distribution.