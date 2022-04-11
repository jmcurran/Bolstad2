#' Test a one sided hypothesis using a sample from a posterior density
#' 
#' Calculates the probability of a one sided null hypothesis from a sample from
#' a posterior density.
#' 
#' This function uses linear interpolation to calculate bounds for points that
#' may not be specified by CDF
#' 
#' @param theta a sample of values from a posterior density
#' @param theta0 the hypothesized value, i.e. H0: theta <= theta0
#' @param type the type of probability to return, 'lower' = Pr(theta <= theta0)
#' or 'upper' = Pr(theta >= theta0). It is sufficient to use 'l' or 'u'
#' @return a list containing the element prob which will be the upper or lower
#' tail probability depending on type
#' @examples
#' 
#' ## The posterior density is N(3,1)
#' theta <- rnorm(1000,3)
#' 
#' ## test whether the true mean is greater than 0 (it is obviously!)
#' pnullSamp(theta)
#' 
#' @export pnullSamp
pnullSamp <- function(theta, theta0 = 0, type = "upper") {

    if (length(theta) < 10)
        stop("theta must have at least ten values")

    if (length(grep("^[lL]", type)) > 0) {
        type <- "lower"
    } else if (length(grep("^[Uu]", type)) > 0) {
        type <- "upper"
    } else {
        stop("type must be one of lower or upper")
    }

    Fx <- ecdf(theta)

    if (type == "lower") {
        prob <- 1 - Fx(theta0)
        cat(paste("Posterior Pr(theta<=theta0) is ", prob, "\n", sep = ""))
        invisible(list(prob = prob))
    } else {
        prob <- Fx(theta0)
        cat(paste("Posterior Pr(theta>=theta0) is ", prob, "\n", sep = ""))
        invisible(list(prob = prob))
    }
}
