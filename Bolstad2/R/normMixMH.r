#' Sample from a normal mixture model using Metropolis-Hastings
#' 
#' normMixMH uses the Metropolis-Hastings algorithm to draw a sample from a
#' univariate target distribution that is a mixture of two normal distributions
#' using an independent normal candidate density or a random walk normal
#' candidate density.
#' 
#' 
#' @param theta0 A vector of length two containing the mean and standard
#' deviation of the first component of the normal mixture
#' @param theta1 A vector of length two containing the mean and standard
#' deviation of the second component of the normal mixture
#' @param p A value between 0 and 1 representing the mixture proportion, so
#' that the true density is \eqn{p\times f(\mu1,\sigma1) + (1-p)\times
#' f(\mu_2,\sigma_2)}
#' @param candidate A vector of length two containing the mean and standard
#' deviation of the candidate density
#' @param steps The number of steps to be used in the Metropolis-Hastings
#' algorithm. steps must be greater than 100
#' @param type Either 'ind' or 'rw' depending on whether a independent
#' candidate density or random walk candidate density is to be used. 'i' and
#' 'r' may be used as alternative compact notation
#' @param startValue A starting value for the chain
#' @param randomSeed A seed for the random number generator. Only used when you
#' want the same sequence of random numbers in the chain
#' @return A vector containing a sample from the normal mixture distribution.
#' @examples
#' 
#' ## Set up the normal mixture
#' theta0 <- c(0,1)
#' theta1 <- c(3,2)
#' p <- 0.8
#' 
#' ## Sample from an independent N(0,3^2) candidate density
#' candidate <- c(0, 3)
#' MCMCsampleInd <- normMixMH(theta0, theta1, p, candidate)
#' 
#' 
#' ## If we wish to use the alternative random walk N(0, 0.5^2)
#' ## candidate density
#' candidate <- c(0, 0.5)
#' MCMCsampleRW <- normMixMH(theta0, theta1, p, candidate, type = 'rw')
#' 
#' @export normMixMH
normMixMH <- function(theta0, theta1, p, candidate, steps = 1000, type = "ind", randomSeed = NULL, startValue = NULL) {
    if (steps < 100) {
        warning("Function should take at least 100 steps")
    }

    if (p <= 0 | p >= 1)
        stop("Mixture proprotion p must be between 0 and 1")

    mu0 <- theta0[1]
    sigma0 <- theta0[2]

    mu1 <- theta1[1]
    sigma1 <- theta1[2]

    mu <- candidate[1]
    sigma <- candidate[2]

    if (any(c(sigma0, sigma0, sigma) <= 0))
        stop("All standard deviations must be strictly non-zero and positive")


    if (length(grep("[Ii]", type)) > 0) {
        type <- "ind"
    } else if (length(grep("[Rr]", type)) > 0) {
        type <- "rw"
    } else {
        stop("Type must be ind or rw")
    }

    theta <- seq(from = min(mu0 - 3 * sigma0, mu1 - 3 * sigma1), to = max(mu0 + 3 * sigma0, mu1 + 3 * sigma1), by = 0.001)

    fx <- p * dnorm(theta, mu0, sigma0) + (1 - p) * dnorm(theta, mu1, sigma1)
    targetSample <- rep(startValue, steps)

    if (type == "rw") {

        if (!is.null(randomSeed))
            set.seed(randomSeed)

        z <- rnorm(steps, mu, sigma)
        u <- runif(steps)

        if (is.null(startValue))
            startValue <- z[1]

        targetSample[1] <- startValue
        g <- rep(0, steps)
        proposal <- rep(0, steps)
        alpha <- rep(0, steps)

        k1 <- p/sigma0 * exp(-0.5 * ((targetSample[1] - mu0)/sigma0)^2)
        k2 <- (1 - p)/sigma1 * exp(-0.5 * ((targetSample[1] - mu1)/sigma1)^2)
        g[1] <- k1 + k2

        i1 <- 1

        for (n in 2:steps) {
            proposal[n] <- targetSample[i1] + z[n]

            k1 <- p/sigma0 * exp(-0.5 * ((proposal[n] - mu0)/sigma0)^2)
            k2 <- (1 - p)/sigma1 * exp(-0.5 * ((proposal[n] - mu1)/sigma1)^2)
            g[n] <- k1 + k2

            k3 <- g[n]
            k4 <- g[i1]

            alpha[n] <- ifelse(k3/k4 > 1, 1, k3/k4)

            ## Metropolis-Hastings Step reject
            if (u[n] >= alpha[n]) {
                targetSample[n] <- targetSample[i1]
            } else {
                ## accept
                targetSample[n] <- proposal[n]
                i1 <- n
            }
        }
    } else {

        if (!is.null(randomSeed))
            set.seed(randomSeed)

        z <- rnorm(steps, mu, sigma)
        u <- runif(steps)

        if (is.null(startValue))
            startValue <- z[1]

        density0 <- dnorm(z, mu, sigma)
        density1 <- dnorm(z, mu0, sigma0)
        density2 <- dnorm(z, mu1, sigma1)

        densityMix <- p * density1 + (1 - p) * density2

        alpha <- rep(0, steps)
        targetSample[1] <- startValue

        i1 <- 1
        for (n in 2:steps) {
            alpha[n] <- density0[i1] * densityMix[n]/(density0[n] * densityMix[i1])
            alpha[n] <- ifelse(alpha[n] > 1, 1, alpha[n])

            ## Metropolis-Hastings Step
            if (u[n] >= alpha[n]) {
                targetSample[n] <- targetSample[i1]
            } else {
                targetSample[n] <- z[n]
                i1 <- n
            }

        }


    }

    oldPar <- par(mfrow = c(1, 2), pty = "s")

    h <- hist(targetSample, plot = FALSE)
    ymax <- max(c(h$density, fx)) * 1.05

    hist(targetSample, prob = TRUE, col = "light blue", xlim = range(theta), ylim = c(0, ymax), main = "Sample from target density", xlab = "x", ylab = "Density")
    lines(theta, fx)
    box()

    plot(targetSample, type = "l", main = "", ylab = "Target Sample")
    par(oldPar)

    invisible(targetSample)
}

