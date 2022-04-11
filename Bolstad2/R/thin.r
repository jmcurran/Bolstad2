#' Thin an MCMC sample
#' 
#' Thins the output from an MCMC process
#' 
#' Note this function does not check to see if k is sensible.
#' 
#' @param x A vector, matrix or data.frame containing output from an MCMC
#' sampling scheme
#' @param k An integer. This function takes every kth element from x
#' @return A thinned vector, matrix or data frame containing every kth element
#' of x.
#' @examples
#' 
#' ## A blockwise Metropolis-Hastings chain of 1000 elements, thinned to
#' ## 5th element
#' ##
#' 
#' MCMCSampleBW <- bivnormMH(0.9, type = 'block')
#' MCMCSampleBW <- thin(MCMCSampleBW, 5)
#' 
#' @export thin
thin <- function(x, k) {
    ## returns every kth element of a vector, matrix, or data.frame

    if (is.vector(x)) {
        n <- length(x)
        idx <- which((1:n)%%k == 0)
        return(x[idx])
    } else if (is.matrix(x) | is.data.frame(x)) {
        nRow <- nrow(x)
        idx <- which((1:nRow)%%k == 0)
        return(x[idx, ])
    } else {
        stop("x must be a vector, a matrix or a data.frame")
    }
}
