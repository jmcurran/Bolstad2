\name{BayesCPH}
\alias{BayesCPH}
\title{Bayesian Cox Proportional Hazards Modelling}
\description{
   Uses a Metropolis Hastings scheme on the proportional hazards model to draw sample from posterior. Uses a matched curvature Student's t candidate generating
 distribution with 4 degrees of freedom to give heavy tails.
}
\usage{BayesCPH(y, t, x, steps = 1000,
                priorMean = NULL, priorVar = NULL,
                mleMean = NULL, mleVar,
                startValue = NULL, randomSeed = NULL,
                plots = FALSE)
}
\arguments{
  \item{y}{the Poisson censored response vector. It has value 0 when the
    variable is censored and 1 when it is not censored.}
  \item{t}{time}
  \item{x}{matrix of covariates}
  \item{steps}{the number of steps to use in the Metropolis-Hastings
    updating}
  \item{priorMean}{the mean of the prior}
  \item{priorVar}{the variance of the prior}
  \item{mleMean}{the mean of the matched curvature likelihood}
  \item{mleVar}{the covariance matrix of the matched curvature
    likelihood}
  \item{startValue}{}
  \item{randomSeed}{a random seed to use for different chains}
  \item{plots}{Plot the time series and auto correlation functions for
    each of the model coefficients}
}
\value{
  A list containing the following components:

  \item{beta}{a data frame containing the sample of the model
    coefficients from the posterior distribution}
  \item{mleMean}{the mean of the matched curvature likelihood. This is
    useful if you've used a training set to estimate the value and wish
    to use it with another data set}
  \item{mleVar}{the covariance matrix of the matched curvature
    likelihood. See mleMean for why you'd want this}
}

