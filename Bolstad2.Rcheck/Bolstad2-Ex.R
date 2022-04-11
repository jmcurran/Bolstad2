pkgname <- "Bolstad2"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "Bolstad2-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Bolstad2')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BayesLogistic")
### * BayesLogistic

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BayesLogistic
### Title: Bayesian Logistic Regression
### Aliases: BayesLogistic

### ** Examples


data(logisticTest.df)
BayesLogistic(logisticTest.df$y, logisticTest.df$x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BayesLogistic", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BayesPois")
### * BayesPois

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BayesPois
### Title: Bayesian Pois Regression
### Aliases: BayesPois

### ** Examples


data(poissonTest.df)
results <- BayesPois(poissonTest.df$y, poissonTest.df$x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BayesPois", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GelmanRubin")
### * GelmanRubin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GelmanRubin
### Title: Calculate the Gelman Rubin statistic
### Aliases: GelmanRubin GR

### ** Examples


## take four chains sampling from a normal mixture density
theta0 <- c(0,1)
theta1 <- c(3,2)
p <- 0.6
candidate <- c(0, 3)

v1 <- normMixMH(theta0, theta1, p, candidate, steps = 200)
v2 <- normMixMH(theta0, theta1, p, candidate, steps = 200)
v3 <- normMixMH(theta0, theta1, p, candidate, steps = 200)
v4 <- normMixMH(theta0, theta1, p, candidate, steps = 200)

theta<-cbind(v1,v2,v3,v4)
GelmanRubin(theta)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GelmanRubin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bivnormMH")
### * bivnormMH

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bivnormMH
### Title: Metropolis Hastings sampling from a Bivariate Normal
###   distribution
### Aliases: bivnormMH

### ** Examples


## independent chain
chain1.df<-bivnormMH(0.9)$targetSample

## random walk chain
chain2.df<-bivnormMH(0.9, type = 'r')$targetSample


## blockwise MH chain
chain3.df<-bivnormMH(0.9, type = 'b')$targetSample

## Gibbs sampling chain
chain4.df<-bivnormMH(0.9, type = 'g')$targetSample

oldPar <- par(mfrow=c(2,2))
plot(y ~ x, type = 'l', chain1.df, main = 'Independent')
plot(y ~ x, type = 'l', chain2.df, main = 'Random Walk')
plot(y ~ x, type = 'l', chain3.df, main = 'Blockwise')
plot(y ~ x, type = 'l', chain4.df, main = 'Gibbs')
par(oldPar)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bivnormMH", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("credInt")
### * credInt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: credInt
### Title: Calculate a credible interval from a numerically specified
###   posterior CDF or from a sample from the posterior
### Aliases: credInt

### ** Examples


## commands for calculating a numerical posterior CDF.
## In this example, the likelihood is proportional to
## \eqn{\theta^{3/2}\times \exp(-\theta/4)} and a N(6, 9) prior is used.
theta <- seq(from = 0.001, to = 40, by = 0.001)
prior <- dnorm(theta,6,3)
ppnLike <- theta^1.5*exp(-theta/4)
ppnPost <- prior*ppnLike
scaleFactor <- sintegral(theta, ppnPost)$int
posterior <- ppnPost/scaleFactor
cdf <- sintegral(theta, posterior)$y
ci<-credInt(theta, cdf)
par(mfrow=c(2,2))
plot(prior ~ theta, type = 'l',  main = 'Prior N(6, 9)')
plot(ppnLike ~ theta, type = 'l', main = 'Proportional likelihood')
plot(posterior ~ theta, type = 'l', main = 'Posterior')
abline(v=c(unlist(ci)))

## Use an inverse method to take a random sample of size 1000
## from the posterior
suppressWarnings(Finv<-approxfun(cdf,theta))
thetaSample<-Finv(runif(1000))
ci<-credInt(thetaSample)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("credInt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("credIntNum")
### * credIntNum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: credIntNum
### Title: Calculate a credible interval from a numerically specified
###   posterior CDF
### Aliases: credIntNum

### ** Examples


## commands for calculating a numerical posterior CDF.
## In this example, the likelihood is proportional to
## \eqn{\theta^{3/2}\times \exp(-\theta/4)} and a N(6, 9) prior is used.
theta <- seq(from = 0.001, to = 40, by = 0.001)
prior <- dnorm(theta,6,3)
ppnLike <- theta^1.5*exp(-theta/4)
ppnPost <- prior*ppnLike
scaleFactor <- sintegral(theta, ppnPost)$int
posterior <- ppnPost/scaleFactor
cdf <- sintegral(theta, posterior)$y
ci<-credIntNum(theta, cdf)
par(mfrow=c(2,2))
plot(prior ~ theta, type = 'l',  main = 'Prior N(6, 9)')
plot(ppnLike ~ theta, type = 'l', main = 'Proportional likelihood')
plot(posterior ~ theta, type = 'l', main = 'Posterior')
abline(v=c(unlist(ci)))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("credIntNum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("credIntSamp")
### * credIntSamp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: credIntSamp
### Title: Calculate a credible interval from a numerically specified
###   posterior CDF
### Aliases: credIntSamp

### ** Examples


## posterior is N(0,1)
theta <- rnorm(1000)
ci<-credIntSamp(theta)
plot(density(theta))
abline(v=c(unlist(ci)))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("credIntSamp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("describe")
### * describe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: describe
### Title: Give simple descriptive statistics for a matrix or a data frame
### Aliases: describe

### ** Examples


data(poissonTest.df)
describe(poissonTest.df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("describe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hierMeanReg")
### * hierMeanReg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hierMeanReg
### Title: Hierarchical Normal Means Regression Model
### Aliases: hierMeanReg

### ** Examples


priorTau <- list(tau0 = 0, v0 = 1000)
priorPsi <- list(psi0 = 500, eta0 = 1)
priorVar <- list(s0 = 500, kappa0 = 1)
priorBeta <- list(b0 = c(0,0), bMat = matrix(c(1000,100,100,1000), ncol = 2))

data(hiermeanRegTest.df)
data.df <- hiermeanRegTest.df
design <- list(y = data.df$y, group = data.df$group,
               x = as.matrix(data.df[,3:4]))
r<-hierMeanReg(design, priorTau, priorPsi, priorVar, priorBeta)

oldPar <- par(mfrow = c(3,3))
plot(density(r$tau))
plot(density(r$psi))
plot(density(r$mu.1))
plot(density(r$mu.2))
plot(density(r$mu.3))
plot(density(r$beta.1))
plot(density(r$beta.2))
plot(density(r$sigmaSq))
par(oldPar)

## example with no covariates
priorTau <- list(tau0 = 0, v0 = 1000)
priorPsi <- list(psi0 = 500, eta0 = 1)
priorVar <- list(s0 = 500, kappa0 = 1)

data(hiermeanRegTest.df)
data.df <- hiermeanRegTest.df
design <- list(y = data.df$y, group = data.df$group, x = NULL)
r<-hierMeanReg(design, priorTau, priorPsi, priorVar)

oldPar <- par(mfrow = c(3,2))
plot(density(r$tau))
plot(density(r$psi))
plot(density(r$mu.1))
plot(density(r$mu.2))
plot(density(r$mu.3))
plot(density(r$sigmaSq))
par(oldPar)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hierMeanReg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("normGibbs")
### * normGibbs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: normGibbs
### Title: Draw a sample from a posterior distribution of data with an
###   unknown mean and variance using Gibbs sampling
### Aliases: normGibbs

### ** Examples


## firstly generate some random data
mu <- rnorm(1)
sigma <- rgamma(1,5,1)
y <- rnorm(100, mu, sigma)

## A \eqn{N(10,3^2)} prior for \eqn{\mu} and a 25 times inverse chi-squared
## with one degree of freedom prior for \eqn{\sigma^2}
MCMCSampleInd <- normGibbs(y, steps = 5000, priorMu = c(10,3),
                           priorVar = c(25,1))


## We can also use a joint conjugate prior for \eqn{\mu} and \eqn{\sigma^2}.
## This will be a \emph{normal}\eqn{(m,\sigma^2/n_0)} prior for \eqn{\mu} given
## the variance \eqn{\sigma^2}, and an \eqn{s0} times an \emph{inverse
## chi-squared} prior for \eqn{\sigma^2}.
MCMCSampleJoint <- normGibbs(y, steps = 5000, type = 'joint',
                             priorMu = c(10,3), priorVar = c(25,1))

## Now plot the results
oldPar <- par(mfrow=c(2,2))

plot(density(MCMCSampleInd$mu),xlab=expression(mu), main =
'Independent')
abline(v=mu)
plot(density(MCMCSampleInd$sig),xlab=expression(sig), main =
'Independent')
abline(v=sigma)

plot(density(MCMCSampleJoint$mu),xlab=expression(mu), main =
'Joint')
abline(v=mu)
plot(density(MCMCSampleJoint$sig),xlab=expression(sig), main =
'Joint')
abline(v=sigma)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normGibbs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("normMixMH")
### * normMixMH

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: normMixMH
### Title: Sample from a normal mixture model using Metropolis-Hastings
### Aliases: normMixMH

### ** Examples


## Set up the normal mixture
theta0 <- c(0,1)
theta1 <- c(3,2)
p <- 0.8

## Sample from an independent N(0,3^2) candidate density
candidate <- c(0, 3)
MCMCsampleInd <- normMixMH(theta0, theta1, p, candidate)


## If we wish to use the alternative random walk N(0, 0.5^2)
## candidate density
candidate <- c(0, 0.5)
MCMCsampleRW <- normMixMH(theta0, theta1, p, candidate, type = 'rw')




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normMixMH", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pNull")
### * pNull

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pNull
### Title: Test a one sided hypothesis from a numerically specified
###   posterior CDF or from a sample from the posterior
### Aliases: pNull

### ** Examples


## commands for calculating a numerical posterior CDF.
## In this example, the likelihood is proportional to
## \eqn{\theta^{3/2}\times \exp(-\theta/4)} and a N(6, 9) prior is used.
theta <- seq(from = 0.001, to = 40, by = 0.001)
prior <- dnorm(theta,6,3)
ppnLike <- theta^1.5*exp(-theta/4)
ppnPost <- prior*ppnLike
scaleFactor <- sintegral(theta, ppnPost)$int
posterior <- ppnPost/scaleFactor
cdf <- sintegral(theta, posterior)$y
pNull(15, theta, cdf)

## Use an inverse method to take a random sample of size 1000
## from the posterior
suppressWarnings(Finv <- approxfun(cdf, theta))
thetaSample<-Finv(runif(1000))
pNull(15, thetaSample)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pNull", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pnullNum")
### * pnullNum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pnullNum
### Title: Test a one sided hypothesis from a numerically specified
###   posterior CDF
### Aliases: pnullNum

### ** Examples


## commands for calculating a numerical posterior CDF.
## In this example, the likelihood is proportional to
## \eqn{\theta^{3/2}\times \exp(-\theta/4)} and a N(6, 9) prior is used.
theta <- seq(from = 0.001, to = 40, by = 0.001)
prior <- dnorm(theta,6,3)
ppnLike <- theta^1.5*exp(-theta/4)
ppnPost <- prior*ppnLike
scaleFactor <- sintegral(theta, ppnPost)$int
posterior <- ppnPost/scaleFactor
cdf <- sintegral(theta, posterior)$y
pnullNum(1, theta, cdf)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pnullNum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pnullSamp")
### * pnullSamp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pnullSamp
### Title: Test a one sided hypothesis using a sample from a posterior
###   density
### Aliases: pnullSamp

### ** Examples


## The posterior density is N(3,1)
theta <- rnorm(1000,3)

## test whether the true mean is greater than 0 (it is obviously!)
pnullSamp(theta)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pnullSamp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sintegral")
### * sintegral

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sintegral
### Title: Numerical integration using Simpson's Rule
### Aliases: sintegral
### Keywords: misc

### ** Examples


## integrate the normal density from -3 to 3
x<-seq(-3,3,length=100)
fx<-dnorm(x)
estimate<-sintegral(x,fx)$int
true.val<-diff(pnorm(c(-3,3)))
cat(paste('Absolute error :',round(abs(estimate-true.val),7),'\n'))
cat(paste('Relative percentage error :', 100*round((abs(estimate-true.val)/true.val),6),'%\n'))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sintegral", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("thin")
### * thin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: thin
### Title: Thin an MCMC sample
### Aliases: thin

### ** Examples


## A blockwise Metropolis-Hastings chain of 1000 elements, thinned to
## 5th element
##

MCMCSampleBW <- bivnormMH(0.9, type = 'block')
MCMCSampleBW <- thin(MCMCSampleBW, 5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("thin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
