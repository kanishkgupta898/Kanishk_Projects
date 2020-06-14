##### Optimization techniques Infividual Project #####

### Model selected - General Linear model ####
# Install package stats4
install.packages("stats4")
library(stats4)

#Generating Random Data
N <- 1000
a <- runif(N)
b <- 5 * a + 3 + rnorm(N)

#Checking model fit
M_fit <- lm(b ~ a)
summary(M_fit)

#Plotting fit
plot(a, b)
abline(M_fit, col = "blue")

#Defining the likelihood function which is based on the concept that
#residuals must have normal distribution
Likelihood_fnc <- function(beta0, beta1, mu, sigma) {
  # Find residuals
  Resi = b - a * beta1 - beta0
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  Resi2 = suppressWarnings(dnorm(Resi, mu, sigma))
  # Sum the log likelihoods for all of the data points
  -sum(log(Resi2))
}

#Checking model fit for M_fit_2.
M_fit_2 <- mle(Likelihood_fnc, start = list(beta0 = 6, beta1 = 4, mu = 2, sigma=4))
M_fit_2

summary(M_fit_2)

#Plotting fit for M_fit_2
plot(a, b)
abline(M_fit_2, col = "blue")

#Checking model fit values for M_fit_2 through different parameters
AIC(M_fit_2)
BIC(M_fit_2)
logLik(M_fit_2)

#Checking model fit for M_fit_3.
M_fit_3 <- mle(Likelihood_fnc, start = list(beta0 = 3, beta1 = 3, sigma=4), fixed = list(mu = 0),
           nobs = length(b))
summary(M_fit_3)

#Plotting fit for M_fit_3
plot(a, b)
abline(M_fit_3, col = "blue")

#Checking model fit values for M_fit_3 through different parameters
AIC(M_fit_3)
BIC(M_fit_3)
logLik(M_fit_3)

#installing packages for mle2
install.packages("bbmle")
library(bbmle)

#Checking model fit for M_fit_4.
M_fit_4 <- mle2(Likelihood_fnc, start = list(beta0 = 3, beta1 = 3, mu = 1, sigma = 4))
summary(M_fit_4)

#Checking model fit values for M_fit_4 through different parameters
AIC(M_fit_4)
BIC(M_fit_4)
logLik(M_fit_4)

#Plotting fit for M_fit_4
plot(a, b)
abline(M_fit_4, col = "blue")

##############################################################################