

#' HIV Survival data
#' 
#' Data from a hypothetical HMO-HIV+ study shown in Table 1.1 of Hosmer, D.W.
#' and Lemeshow, S. (1998) Applied Survival Analysis: Regression Modeling of
#' Time to Event Data, John Wiley and Sons Inc., New York, NY
#' 
#' 
#' @name AidsSurvival.df
#' @docType data
#' @format A data frame with 100 observations on 7 variables. \tabular{rlll}{
#' [,1] \tab id \tab numeric \tab l Subject ID code \cr [,2] \tab entdate \tab
#' date \tab Entry date (ddmmyr) \cr [,3] \tab enddate \tab date \tab Entry
#' date (ddmmyr) \cr [,4] \tab time \tab numeric \tab Survival Time = days
#' between Entry date and End date \cr [,5] \tab age \tab numeric \tab Age in
#' years \cr [,6] \tab drug \tab factor \tab History of IV drug use (0 = No, 1
#' = Yes) \cr [,7] \tab censor \tab factor \tab Follow-Up Status1 = Death due
#' to AIDS or AIDS \cr \tab \tab \tab related factors (0 = Alive at study end
#' or lost to follow-up)\cr }
#' @keywords datasets
NULL





#' Chapter 10 Example 16 data
#' 
#' A random sample of size 10 from a \eqn{N(\mu, \sigma^{2})} distribution
#' where both mu and sigma are unknown parameters.
#' 
#' 
#' @name c10ex16.df
#' @aliases c10ex16.df ex16.df
#' @docType data
#' @format A data frame with 10 observations in a single variable called y
#' @keywords datasets
NULL





#' Coronary Heart Disease Chapter 8 Example 11
#' 
#' The age and coronory heart disease status of 100 individuals taken from
#' Hosmer and Lemeshow (1989).
#' 
#' 
#' @name chd.df
#' @docType data
#' @format A data frame with 100 observations in two columns \tabular{lrrr}{
#' [,1] \tab age \tab numeric \tab age in years \cr [,2] \tab chd \tab numeric
#' factor \tab coronary heat disease status. Levels (1 = Yes), (0 = No) \cr }
#' @keywords datasets
NULL





#' Test data for hiermeanReg
#' 
#' Data for testing hiermeanReg which uses Gibbs sampling on a hierarchical
#' normal mean model with regression on covariates
#' 
#' 
#' @name hiermeanRegTest.df
#' @docType data
#' @format A data frame with 30 observations on 4 variables.  \tabular{rlll}{\
#' [1,] \tab y \tab numeric \tab the response vector \cr [2,] \tab group \tab
#' factor \tab the grouping factor levels 1-3 \cr [3,] \tab x1 \tab numeric
#' \tab the first covariate \cr [4,] \tab x2 \tab numeric \tab the second
#' covariate \cr }
#' @seealso hiermeanReg
#' @keywords datasets
NULL





#' Test data for bayesLogistic
#' 
#' A test data set for bayesLogisticReg
#' 
#' 
#' @name logisticTest.df
#' @docType data
#' @format A data frame with 100 observations on 6 variables.  \tabular{rlll}{
#' [1,] \tab x \tab numeric \tab the covariate \cr [2,] \tab eps \tab numeric
#' \tab the error in the response \cr [3,] \tab logit.p \tab numeric \tab the
#' logit of the probability of success given x = 2 + 3*x + eps \cr [4,] \tab p
#' \tab numeric \tab the probability of success given x \cr [5,] \tab u \tab
#' numeric \tab a U[0,1] random variable \cr [6,] \tab y \tab binary \tab if
#' u[i]<p[i] = 1, otherwise 0 }
#' @seealso bayesLogistic
#' @keywords datasets
NULL





#' A test data set for bayesPois
#' 
#' A test data set for bayesPois. The data come from the equation
#' \eqn{\log(\lambda_{i}) = 1 + 5x_{i} + \epsilon_{i}} where \eqn{\epsilon_{i}}
#' comes from N(0,0.01).
#' 
#' 
#' @name poissonTest.df
#' @docType data
#' @format A data frame with 100 observations on 5 variables.  \tabular{rlll}{
#' [1,] \tab x \tab numeric \tab the covariate \cr [2,] \tab eps \tab numeric
#' \tab the error in the log response \cr [3,] \tab log.lam \tab numeric \tab
#' \eqn{\log(\lambda_{i}) = 1 + 5x_{i} + \epsilon_{i}} where \eqn{\epsilon_{i}}
#' \cr [4,] \tab lam \tab numeric \tab \eqn{\exp(\log(\lambda))} \cr [5,] \tab
#' y \tab numeric \tab a Poisson random variate with mean \eqn{\lambda_{i}} \cr
#' }
#' @seealso bayesPois
#' @keywords datasets
NULL

## usethis namespace: start
#' @importFrom graphics box
#' @importFrom graphics hist
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom stats acf
#' @importFrom stats approx
#' @importFrom stats approxfun
#' @importFrom stats coef
#' @importFrom stats cov
#' @importFrom stats dnorm
#' @importFrom stats dt
#' @importFrom stats ecdf
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats rchisq
#' @importFrom stats rnorm
#' @importFrom stats rt
#' @importFrom stats runif
#' @importFrom stats sd
#' @importFrom stats ts
#' @importFrom stats var
## usethis namespace: end
NULL



