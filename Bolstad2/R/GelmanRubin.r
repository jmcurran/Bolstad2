#' Calculate the Gelman Rubin statistic
#' 
#' Calculate the Gelman Rubin statistic
#' 
#' 
#' @aliases GelmanRubin GR
#' @param theta A matrix containing samples from at least two chains on a
#' parameter theta. Each chain should 2n iterations. The last n iterations will
#' be used to calculate the statistic
#' @return A list containing n, the between chain variance B, the within chain
#' variance W, the estimated variance of the parameter vHat, and the Gelman
#' Rubin statistic \eqn{R = \sqrt{vHat/W}}
#' @references Gelman, A. and Rubin, D.B. (1992) 'Inference from iterative
#' simulations using multiple sequences with discussion.' Statistical Science
#' 8, pp. 457-511
#' @examples
#' 
#' ## take four chains sampling from a normal mixture density
#' theta0 <- c(0,1)
#' theta1 <- c(3,2)
#' p <- 0.6
#' candidate <- c(0, 3)
#' 
#' v1 <- normMixMH(theta0, theta1, p, candidate, steps = 200)
#' v2 <- normMixMH(theta0, theta1, p, candidate, steps = 200)
#' v3 <- normMixMH(theta0, theta1, p, candidate, steps = 200)
#' v4 <- normMixMH(theta0, theta1, p, candidate, steps = 200)
#' 
#' theta<-cbind(v1,v2,v3,v4)
#' GelmanRubin(theta)
#' 
#' @export GelmanRubin
GelmanRubin <- function(theta){
    ## theta is a matrix of outputs from various chains

    if(!is.matrix(theta)){
        stop("theta must be a matrix")
    }

    nObs <- nrow(theta)
    nCols <- ncol(theta)
    n1 <- floor(nObs*0.5)
    n2 <- nObs - n1

    if(nObs<100)
        stop("There must be at least 100 observations from each chain")

    if(nCols<2)
        stop("There must be at least two chains")

    theta <- theta[-(1:n1),] # take only the second half of the data

    vars <- apply(theta, 2, var)
    means  <- apply(theta, 2, mean)
    mBar <- mean(means)

    B <- n2*sum((means-mBar)^2)/(nCols-1)
    W <- sum(vars)/nCols
    sigmaSq <- ((n2-1)*W+B)/(n2)
    vHat <- sigmaSq+B/(n2*nCols)
    df <- n2
    R <- sqrt(vHat/W*(df/(df-2)))

    results.df <- data.frame(n = n2,B,W,vHat,R)
    cat(paste(R,"\n"))
    invisible(results.df)
}

GR<-function(theta){
    return(GelmanRubin(theta))
}
