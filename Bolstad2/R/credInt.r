#' Calculate a credible interval from a numerically specified posterior CDF or
#' from a sample from the posterior
#' 
#' Calculates a lower, upper, or two-sided credible interval from the numerical
#' posterior CDF or from a sample from the posterior.
#' 
#' This function uses linear interpolation to calculate bounds for points that
#' may not be specified by CDF
#' 
#' @param theta either a sample from the posterior density or the values over
#' which the the posterior CDF is specified
#' @param cdf the values of the CDF, \eqn{F(\theta) =
#' \int_{-\infty}^{\theta}f(t).df} where \eqn{f(t)} is the PDF. This only needs
#' to be specified if a numerically specified posterior is being used
#' @param conf the desired 'confidence' level
#' @param type the type of interval to return, 'lower' = one sided lower bound,
#' 'two-sided' = two - sided, or 'upper' = one sided upper bound. It is
#' sufficient to use 'l','t' or 'u'
#' @return a list containing the elements lower.bound, uppper.bound or both
#' depending on type
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
#' ci<-credInt(theta, cdf)
#' par(mfrow=c(2,2))
#' plot(prior ~ theta, type = 'l',  main = "Prior N(6, 9)")
#' plot(ppnLike ~ theta, type = 'l', main = "Proportional likelihood")
#' plot(posterior ~ theta, type = 'l', main = "Posterior")
#' abline(v=c(unlist(ci)))
#' 
#' ## Use an inverse method to take a random sample of size 1000
#' ## from the posterior
#' suppressWarnings(Finv<-approxfun(cdf,theta))
#' thetaSample<-Finv(runif(1000))
#' ci<-credInt(thetaSample)
#' 
#' @export credInt
credInt<-function(theta, cdf = NULL, conf = 0.95, type="twosided"){

    if(conf <=0  | conf >=1)
        stop("conf must be between 0 and 1")

    if(length(grep('^[Ll]',type))>0){
        type<-'lower'
    }else if(length(grep('^[Tt]',type))>0){
        type<-'twosided'
    }else if(length(grep('^[Uu]',type))>0){
        type<-'upper'
    }else{
        stop("type must be one of lower, upper or twosided")
    }

    alpha<-1-conf
    n<-length(theta)

    if(n<10)
        stop("theta must have at least ten values")

    if(!is.null(cdf)){
        suppressWarnings(Finv<-approxfun(cdf, theta))

        if(type=='lower'){
            lower.bound = Finv(alpha)
            cat(paste("Lower credible bound is : ", lower.bound, "\n",sep=""))
            invisible(list(lower.bound=lower.bound))
        }else if(type=='upper'){
            upper.bound<-Finv(1-alpha)
            cat(paste("Upper credible bound is : ", upper.bound, "\n",sep=""))
            invisible(list(upper.bound=upper.bound))
        }else{
            lower.bound = Finv(alpha/2)
            upper.bound<-Finv(1-alpha/2)
            cat(paste("Credible interval is : (", lower.bound,
                      ",", upper.bound,")\n",sep=""))
            invisible(list(lower.bound=lower.bound,upper.bound=upper.bound))
        }
    }else{
        if(type=='lower'){
            lower.bound <- quantile(theta, alpha)
            cat(paste("Lower credible bound is ",
                      lower.bound, "\n", sep=""))
            invisible(list(lower.bound=lower.bound))
        }else if(type=='upper'){
            upper.bound <- quantile(theta, 1-alpha)
            cat(paste("Upper credible bound is ",
                      upper.bound, "\n", sep=""))
            invisible(list(upper.bound=upper.bound))
        }else{
            bounds<-quantile(theta, c(alpha/2,1-alpha/2))
            lower.bound <-bounds[1]
            upper.bound <-bounds[2]
            cat(paste("Credible interval is (",
                      lower.bound, ',', upper.bound,")\n", sep=""))
            invisible(list(lower.bound=lower.bound,upper.bound=upper.bound))
        }
    }
}





