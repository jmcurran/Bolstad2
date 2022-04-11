\name{sintegral}
\alias{sintegral}
\title{Numerical integration using Simpson's Rule}
\description{
  Takes a vector of \eqn{x} values and a corresponding set of postive
  \eqn{f(x)=y} values and evaluates the area under the curve:
  \deqn{
    \int{f(x)dx}
    }.
}
\usage{
	sintegral(x, fx, n.pts = 256)
}
\arguments{
	\item{x}{a sequence of \eqn{x} values.}
	\item{fx}{the value of the function to be integrated at \eqn{x}.}
	\item{n.pts}{the number of points to be used in the
	  integration.}
}
\value{
  returns a list with the following elements
  \item{x}{the x-values at which the integral has been evaluated}
  \item{y}{the cummulative integral}
  \item{int}{the value of the integral over the whole range}
}
\examples{
## integrate the normal density from -3 to 3
x<-seq(-3,3,length=100)
fx<-dnorm(x)
estimate<-sintegral(x,fx)$int
true.val<-diff(pnorm(c(-3,3)))
cat(paste("Absolute error :",round(abs(estimate-true.val),7),"\n"))
cat(paste("Relative percentage error :", 100*round((abs(estimate-true.val)/true.val),6),"\%\n"))
}
\keyword{misc}


