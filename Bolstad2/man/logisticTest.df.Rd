\name{logisticTest.df}
\docType{data}
\alias{logisticTest.df}
\title{Test data for bayesLogistic}
\description{
  A test data set for bayesLogisticReg
}
\usage{data(logisticTest.df)
}
\format{
  A data frame with 100 observations on 6 variables.
  \tabular{rlll}{
    [1,] \tab x \tab numeric \tab the covariate \cr
    [2,] \tab eps \tab numeric \tab the error in the response \cr
    [3,] \tab logit.p \tab numeric \tab the logit of the probability of
    success given x = 2 + 3*x + eps \cr
    [4,] \tab p \tab numeric \tab the probability of success given x \cr
    [5,] \tab u \tab numeric \tab a U[0,1] random variable \cr
    [6,] \tab y \tab binary \tab if u[i]<p[i] = 1, otherwise 0
  }
}
\seealso{bayesLogistic
}
\keyword{datasets}
