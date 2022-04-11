\name{normGibbs}
\alias{normGibbs}
\title{Draw a sample from a posterior distribution of data with an
  unknown mean and variance using Gibbs sampling}
\description{normGibbs draws a Gibbs sample from the posterior distribution
of the parameters given the data fron normal distribution with
 unknown mean and variance.  The prior for \eqn{\mu} given \eqn{var} is
 prior mean \eqn{m0} and prior variance \eqn{var/n0} .  That means
 \eqn{n0} is the 'equivalent sample size.' The prior distribution of
 the variance is \eqn{s0} times an inverse chi-squared with \eqn{kappa0}
 degrees of freedom. The joint prior is the product  \eqn{g(var)g(mu|var)}.
}
\usage{
normGibbs(y, steps = 1000, type = 'ind', ...)
}
\arguments{
  \item{y}{A vector containing the data}
  \item{steps}{The number of iterations of Gibbs sampling to carry out}
  \item{type}{Either 'ind' for sampling from an independent conjugate prior or
    'joint' for sampling from a joint conjugate prior. 'i' and 'j' can be used
    as compact notation}
  \item{\dots}{If type = 'ind' then the user can specify the prior for
    \eqn{\mu} with a parameter priorMu which can either be a single
    number m0, or m0 and n0. if m0 and n0 are not specified then m0 and
    n0 are 0 by default. The user can also specify priorVar, which if
    given, must be a vector with two elements s0 and kappa0. If s0 and
    kappa0 are not given then they are zero by default. If type =
    'joint' then priorMu must be a vector of length two with elements m0
    and sd0.  The user can also specify priorVar, which if given, must
    be a vector with two elements s0 and kappa0. If s0 and kappa0 are
    not given then they are zero by default.}
}
\value{
  A data frame containing three variables
   \tabular{rll}{
    [1,] \tab mu \tab a sample from the posterior distribution of the
    mean \cr
    [2,] \tab sig \tab a sample from the posterior distribution of
    the standard deviation \cr
    [3,] \tab mu \tab a sample from the posterior distribution
	    of the variance = sig^2 \cr
  }
}
\examples{
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
"Independent")
abline(v=mu)
plot(density(MCMCSampleInd$sig),xlab=expression(sig), main =
"Independent")
abline(v=sigma)

plot(density(MCMCSampleJoint$mu),xlab=expression(mu), main =
"Joint")
abline(v=mu)
plot(density(MCMCSampleJoint$sig),xlab=expression(sig), main =
"Joint")
abline(v=sigma)

}
\author{James M. Curran}

