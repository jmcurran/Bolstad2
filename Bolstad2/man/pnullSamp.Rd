\name{pnullSamp}
\alias{pnullSamp}
\title{Test a one sided hypothesis using a sample from a posterior density}
\description{
 Calculates the probability of a one sided null hypothesis from a sample
 from a posterior density.
}
\usage{pnullSamp(theta, theta0 = 0, type = 'upper')
}
\arguments{
\item{theta}{a sample of values from a posterior density}
\item{theta0}{the hypothesized value, i.e. H0: theta <= theta0}
\item{type}{the type of probability to return, 'lower' = Pr(theta <=
theta0) or 'upper' = Pr(theta >= theta0). It is sufficient to use 'l' or
'u'}
}
\value{
  a list containing the element prob which will be the upper or lower
tail probability depending on type
}
\details{
  This function uses linear interpolation to calculate bounds for points
  that may not be specified by CDF
}
\examples{
## The posterior density is N(3,1)
theta <- rnorm(1000,3)

## test whether the true mean is greater than 0 (it is obviously!)
pnullSamp(theta)
}

