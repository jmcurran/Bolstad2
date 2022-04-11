\name{hierMeanReg}
\alias{hierMeanReg}
\title{Hierarchical Normal Means Regression Model}
\description{fits a hierarchical normal model of the form \eqn{E[y_{ij}]
    = \mu_{j} + \beta_{1}x_{i1}+\dots+\beta_{p}x_{ip}}
}
\usage{
hierMeanReg(design, priorTau, priorPsi, priorVar,
            priorBeta = NULL, steps = 1000, startValue = NULL,
            randomSeed = NULL)
}
\arguments{
  \item{design}{a list with elements y = response vector, group  =
    grouping vector, x = matrix of covariates or NULL if there are no
    covariates}
  \item{priorTau}{a list with elements tau0 and v0}
  \item{priorPsi}{a list with elements psi0 and eta0}
  \item{priorVar}{a list with elements s0 and kappa0}
  \item{priorBeta}{a list with elements b0 and bMat or NULL if x is
    NULL}
  \item{steps}{the number of Gibbs sampling steps to take}
  \item{startValue}{a list with possible elements tau, psi, mu, sigmasq
    and beta. tau, psi and sigmasq must all be scalars. mu and beta must
    be vectors with as many elements as there are groups and covariates
    respectively}
  \item{randomSeed}{a random seed for the random number generator}
}
\value{
  A data frame with variables:
  \item{tau}{Samples from the posterior distribution of tau}
  \item{psi}{Samples from the posterior distribution of psi}
  \item{mu}{Samples from the posterior distribution of mu}
  \item{beta}{Samples from the posterior distribution of beta if there
    are any covariates}
  \item{sigmaSq}{Samples from the posterior distribution of \eqn{\sigma^2}}
  \item{sigma}{Samples from the posterior distribution of sigma}
}
\examples{
priorTau <- list(tau0 = 0, v0 = 1000)
priorPsi <- list(psi0 = 500, eta0 = 1)
priorVar <- list(s0 = 500, kappa0 = 1)
priorBeta <- list(b0 = c(0,0), bMat = matrix(c(1000,100,100,1000), nc = 2))

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

}

