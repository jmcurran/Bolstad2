#' Calculate a credible interval from a numerically specified posterior CDF
#' 
#' Calculates a lower, upper, or two-sided credible interval from the numerical
#' posterior CDF.
#' 
#' This function uses linear interpolation to calculate bounds for points that
#' may not be specified by CDF
#' 
#' @param theta a sample from the posterior density
#' @param conf the desired 'confidence' level
#' @param type the type of interval to return, 'lower' = one sided lower bound,
#' 'two-sided' = two - sided, or 'upper' = one sided upper bound. It is
#' sufficient to use 'l','t' or 'u'
#' @return a list containing the elements lower.bound, uppper.bound or both
#' depending on type
#' @examples
#' 
#' ## posterior is N(0,1)
#' theta <- rnorm(1000)
#' ci<-credIntSamp(theta)
#' plot(density(theta))
#' abline(v=c(unlist(ci)))
#' 
#' @export credIntSamp
credIntSamp <- function(theta, conf = 0.95, type = "twosided") {

    if (length(theta) < 10)
        warning("theta is a very small sample, therefore the results may not be accurate")

    if (conf <= 0 | conf >= 1)
        stop("conf must be between 0 and 1")

    if (length(grep("^[Ll]", type)) > 0) {
        type <- "lower"
    } else if (length(grep("^[Uu]", type)) > 0) {
        type <- "upper"
    } else if (length(grep("^[Tt]", type)) > 0) {
        type <- "twosided"
    } else {
        stop("Type must be one of lower, upper or twosided")
    }

    alpha <- 1 - conf

    if (type == "lower") {
        lower.bound <- quantile(theta, alpha)
        cat(paste("Lower credible bound is ", lower.bound, "\n", sep = ""))
        invisible(list(lower.bound = lower.bound))
    } else if (type == "upper") {
        upper.bound <- quantile(theta, 1 - alpha)
        cat(paste("Upper credible bound is ", upper.bound, "\n", sep = ""))
        invisible(list(upper.bound = upper.bound))
    } else {
        bounds <- quantile(theta, c(alpha/2, 1 - alpha/2))
        lower.bound <- bounds[1]
        upper.bound <- bounds[2]
        cat(paste("Credible interval is (", lower.bound, ",", upper.bound, ")\n", sep = ""))
        invisible(list(lower.bound = lower.bound, upper.bound = upper.bound))
    }
}
