\name{credIntNum}
\alias{credIntNum}
\title{Calculate a credible interval from a numerically specified
  posterior CDF}
\description{
  Calculates a lower, upper, or two-sided credible interval
  from the numerical posterior CDF.
}
\usage{credIntNum(theta, cdf, conf = 0.95, type="twosided")
}
\arguments{
  \item{theta}{the values over which the the posterior CDF is specified}
  \item{cdf}{the values of the CDF, \eqn{F(\theta) =
      \int_{-\infty}^{\theta}f(t).df} where \eqn{f(t)} is the PDF.}
  \item{conf}{the desired 'confidence' level}
  \item{type}{ the type of interval to return, 'lower' = one sided lower
    bound, 'two-sided' = two - sided, or 'upper' = one sided upper
    bound. It is sufficient to use 'l','t' or 'u'}
}
\value{
  a list containing the elements lower.bound, uppper.bound or both
  depending on type
}
\details{
  This function uses linear interpolation to calculate bounds for points
  that may not be specified by CDF
}
\examples{
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
plot(prior ~ theta, type = 'l',  main = "Prior N(6, 9)")
plot(ppnLike ~ theta, type = 'l', main = "Proportional likelihood")
plot(posterior ~ theta, type = 'l', main = "Posterior")
abline(v=c(unlist(ci)))
}

