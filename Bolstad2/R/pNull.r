#' Test a one sided hypothesis from a numerically specified posterior CDF or
#' from a sample from the posterior
#' 
#' Calculates the probability of a one sided null hypothesis from a numerically
#' calculated posterior CDF or from a sample from the posterior.
#' 
#' This function uses linear interpolation to calculate bounds for points that
#' may not be specified by CDF
#' 
#' @param theta0 the hypothesized value, i.e. H0: theta <= theta0
#' @param theta a sample of values from the posterior density, or, if cdf is
#' not NULL then the values over which the the posterior CDF is specified
#' @param cdf the values of the CDF, \eqn{F(\theta) =
#' \int_{-\infty}^{\theta}f(t).df} where \eqn{f(t)} is the PDF.
#' @param type the type of probability to return, 'lower' = Pr(theta <= theta0)
#' or 'upper' = Pr(theta >= theta0). It is sufficient to use 'l' or 'u'
#' @return a list containing the element prob which will be the upper or lower
#' tail probability depending on type
#' @examples
#' 
#' ## commands for calculating a numerical posterior CDF.
#' ## In this example, the likelihood is proportional to
#' ## \eqn{\theta^{3/2}\times \exp(-\theta/4)} and a N(6, 9) prior is used.
#' theta <- seq(from = 0.001, to = 40, by = 0.001)
#' prior <- dnorm(theta,6,3)
#' ppnLike <- theta^1.5*exp(-theta/4)
#' ppnPost <- prior*ppnLike
#' scaleFactor <- sintegral(theta, ppnPost)$int
#' posterior <- ppnPost/scaleFactor
#' cdf <- sintegral(theta, posterior)$y
#' pNull(15, theta, cdf)
#' 
#' ## Use an inverse method to take a random sample of size 1000
#' ## from the posterior
#' suppressWarnings(Finv <- approxfun(cdf, theta))
#' thetaSample<-Finv(runif(1000))
#' pNull(15, thetaSample)
#' 
#' @export pNull
pNull<-function(theta0, theta, cdf = NULL, type = 'upper'){

    if(length(theta)<10)
        stop("theta must have at least ten values")

    if(length(grep('^[lL]',type))>0){
        type<-'lower'
    }else if(length(grep('^[Uu]',type))>0){
        type<-'upper'
    }else{
        stop("type must be one of lower or upper")
    }

    Fx<-ecdf(theta)

    if(!is.null(cdf))
    {
        o <- order(theta)
        if(any(theta[o]!=theta)){
            warning("theta is not in ascending order. This may cause problems")
        }
        suppressWarnings(Fx<-approxfun(theta,cdf))
    }

    if(type=='lower'){
        prob<-Fx(theta0)
        cat(paste("Posterior Pr(theta<=theta0) is ",
                  prob, "\n",sep=""))
        invisible(list(prob=prob))
    }else{
        prob<-1-Fx(theta0)
        cat(paste("Posterior Pr(theta>=theta0) is ",
                  prob, "\n",sep=""))
        invisible(list(prob=prob))
    }
}
