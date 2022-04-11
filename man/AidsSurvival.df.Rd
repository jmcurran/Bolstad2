\name{AidsSurvival.df}
\docType{data}
\alias{AidsSurvival.df}
\title{HIV Survival data}
\description{
Data from a hypothetical HMO-HIV+ study shown in
Table 1.1 of Hosmer, D.W. and Lemeshow, S. (1998) Applied Survival
Analysis: Regression Modeling of Time to Event Data, John Wiley and Sons
Inc., New York, NY
}

\usage{data (AidsSurvival.df)}
\format{
A data frame with 100 observations on 7 variables.
\tabular{rlll}{
[,1] \tab id \tab numeric \tab l Subject ID code \cr
[,2] \tab entdate \tab date \tab Entry date (ddmmyr) \cr
[,3] \tab enddate \tab date \tab Entry date (ddmmyr) \cr
[,4] \tab time \tab numeric \tab Survival Time =  days between Entry date and End date \cr
[,5] \tab age \tab numeric \tab Age in years \cr
[,6] \tab drug \tab factor \tab History of IV drug use (0 = No, 1 = Yes) \cr
[,7] \tab censor \tab factor \tab Follow-Up Status1 = Death due to AIDS or AIDS \cr
\tab \tab \tab related factors (0 = Alive at study end or lost to follow-up)\cr
}
}
\keyword{datasets}






