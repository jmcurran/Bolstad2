#' Test a one sided hypothesis from a numerically specified posterior CDF
#' 
#' Calculates the probability of a one sided null hypothesis from a numerically
#' calculated posterior CDF.
#' 
#' This function uses linear interpolation to calculate bounds for points that
#' may not be specified by CDF
#' 
#' @param theta0 the hypothesized value, i.e. H0: theta <= theta0
#' @param theta the values over which the the posterior CDF is specified
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
#' pnullNum(1, theta, cdf)
#' 
#' @export pnullNum
pnullNum<-function(theta0, theta, cdf, type = 'upper'){

    if(length(theta)<10)
        stop("theta must have at least ten values")

    if(length(grep('^[lL]',type))>0){
        type<-'lower'
    }else if(length(grep('^[Uu]',type))>0){
        type<-'upper'
    }else{
        stop("type must be one of lower or upper")
    }

    Fx<-approxfun(theta,cdf)

    if(type=='lower'){
        prob<-1-Fx(theta0)
        cat(paste("Posterior Pr(theta<=theta0) is ",
                  prob, "\n",sep=""))
        invisible(list(prob=prob))
    }else{
        prob<-Fx(theta0)
        cat(paste("Posterior Pr(theta>=theta0) is ",
                  prob, "\n",sep=""))
        invisible(list(prob=prob))
    }
}
