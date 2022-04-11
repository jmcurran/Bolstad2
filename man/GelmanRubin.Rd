\name{GelmanRubin}
\alias{GelmanRubin}
\alias{GR}
\title{Calculate the Gelman Rubin statistic}
\description{Calculate the Gelman Rubin statistic
}
\usage{
GelmanRubin(theta)
GR(theta)
}
\arguments{
  \item{theta}{A matrix containing samples from at least two chains on a
    parameter theta. Each chain should 2n iterations. The last n
    iterations will be used to calculate the statistic}
}
\value{
  A list containing n, the between chain variance B, the within chain
  variance W, the estimated variance of the parameter vHat, and the
  Gelman Rubin statistic \eqn{R = \sqrt{vHat/W}}
}
\references{
  Gelman, A. and Rubin, D.B. (1992) 'Inference from iterative simulations using multiple sequences with discussion.' Statistical Science 8, pp. 457-511
}

\examples{
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
}