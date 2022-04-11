#' Bayesian Pois Regression
#' 
#' Performs Metropolis Hastings on the logistic regression model to draw sample
#' from posterior. Uses a matched curvature Student's t candidate generating
#' distribution with 4 degrees of freedom to give heavy tails.
#' 
#' 
#' @param y the binary response vector
#' @param x matrix of covariates
#' @param steps the number of steps to use in the Metropolis-Hastings updating
#' @param priorMean the mean of the prior
#' @param priorVar the variance of the prior
#' @param mleMean the mean of the matched curvature likelihood
#' @param mleVar the covariance matrix of the matched curvature likelihood
#' @param startValue a vector of starting values for all of the regression
#' coefficients including the intercept
#' @param randomSeed a random seed to use for different chains
#' @param plots Plot the time series and auto correlation functions for each of
#' the model coefficients
#' @return A list containing the following components:
#' 
#' \item{beta}{a data frame containing the sample of the model coefficients
#' from the posterior distribution} \item{mleMean}{the mean of the matched
#' curvature likelihood. This is useful if you've used a training set to
#' estimate the value and wish to use it with another data set}
#' \item{mleVar}{the covariance matrix of the matched curvature likelihood. See
#' mleMean for why you'd want this}
#' @examples
#' 
#' data(poissonTest.df)
#' results <- BayesPois(poissonTest.df$y, poissonTest.df$x)
#' 
#' @export BayesPois
BayesPois <- function(y, x, steps = 1000, priorMean = NULL, priorVar = NULL, mleMean = NULL, mleVar, startValue = NULL, randomSeed = NULL, plots = FALSE) {

    if (!is.null(randomSeed))
        set.seed(randomSeed)

    nObs <- length(y)

    if (is.vector(x))
        x <- as.matrix(x, ncol = 1)

    nParameters <- ncol(x) + 1  ## number of covariates + intercept

    if (!is.null(startValue)) {
        if (length(startValue) < nParameters) {
            stop("You must have as many starting values as you have model parameters")
        }
    }

    ## inital mean of the matched curvature likelihood
    if (is.null(mleMean))
        mleMean <- c(log(mean(y)), rep(0, nParameters - 1))

    X <- cbind(rep(1, nObs), x)
    Xt <- t(X)


    calcMatchedCurvatureNormLike <- function() {

        betaX <- X %*% mleMean
        Mu <- exp(betaX)
        Vdiag <- Mu
        Y <- betaX + (y - Mu)/Mu

        ## I have no idea why the diag command doesn't work as it should: e.g.  Vyinv <- diag(Vdiag, nrow = length(Vdiag)) therefore this two-step procedure is needed
        Vyinv <- matrix(0, nrow = nObs, ncol = nObs)
        diag(Vyinv) <- Vdiag

        XtV <- Xt %*% Vyinv
        VLinv <- XtV %*% X
        VL <- solve(VLinv)
        w1 <- VL %*% XtV
        mleMean <- w1 %*% Y


        ## Loop iterations to converge to MLE
        for (k in 1:20) {
            betaX <- X %*% mleMean
            Mu <- exp(betaX)
            Vdiag <- Mu
            Y <- betaX + (y - Mu)/Mu
            Vyinv <- matrix(0, nrow = nObs, ncol = nObs)
            diag(Vyinv) <- Vdiag

            XtV <- Xt %*% Vyinv
            VLinv <- XtV %*% X
            VL <- solve(VLinv)
            w1 <- VL %*% XtV
            mleMean <- w1 %*% Y
        }

        return(list(mleMean = mleMean, mleVar = VL))
    }  ## calcMatchedCurvatureNormLike

    normApproxPosterior <- function() {

        result <- list(postMean = rep(0, nParameters), postVar = matrix(0, ncol = nParameters, nrow = nParameters))



        ## if the prior mean and variance isn't specified then set it equal to the mle mean and variance
        if (is.null(priorMean) & is.null(priorVar)) {
            result$postMean <- mleMean
            result$postVar <- mleVar
        } else {
            mleVarInv <- solve(mleVar)
            priorVarInv <- solve(priorVar)
            postPrec <- mleVarInv + priorVarInv
            result$postVar <- solve(postPrec)

            w2 <- postVar %*% priorVarInv
            w4 <- w2 * priorMean
            w3 <- postVar %*% mleVarInv
            w5 <- w3 * mleMean
            result$postMean <- w4 + w5
        }

        return(result)
    }

    mleParams <- calcMatchedCurvatureNormLike()
    mleMean <- mleParams$mleMean
    mleVar <- mleParams$mleVar

    posterior <- normApproxPosterior()
    postMean <- posterior$postMean
    postVar <- posterior$postVar

    U <- chol(postVar)

    candBeta <- matrix(rt(steps * nParameters, df = 4), ncol = nParameters)

    if (!is.null(startValue))
        candBeta[1, ] <- startValue

    WM2 <- candBeta %*% U
    WM3 <- matrix(rep(postMean, rep(steps, nParameters)), ncol = nParameters)
    WM4 <- WM2 + WM3
    V2 <- cov(WM4)

    ft0 <- apply(dt(candBeta, df = 4), 1, prod)
    ftn <- apply(dnorm(candBeta), 1, prod)
    q1 <- ft0/1

    ## Metropolis-Hastings

    BetaXt <- WM4 %*% Xt

    BetaXt <- exp(BetaXt)

    for (j in 1:nObs) BetaXt[, j] <- -BetaXt[, j] + y[j] * log(BetaXt[, j])

    logg1 <- rowSums(BetaXt)
    logg1 <- logg1 - max(logg1)
    # g1 <- exp(logg1)

    logq1 <- log(q1)

    u <- runif(steps)
    i1 <- 1

    betaSample <- WM4

    for (n in 2:steps) {
        alpha <- exp(logq1[i1] + logg1[n] - logq1[n] - logg1[i1])
        alpha <- ifelse(alpha > 1, 1, alpha)

        if (u[n] >= alpha) {
            ## reject
            betaSample[n, ] <- WM4[i1, ]
        } else {
            betaSample[n, ] <- WM4[n, ]
            i1 <- n
        }
    }

    beta.df <- data.frame(betaSample)
    names(beta.df) <- paste("b", 0:(ncol(beta.df) - 1), sep = "")
    describe(beta.df)

    Mean.beta <- sapply(beta.df, mean)
    StdDev.beta <- sapply(beta.df, sd)
    Z.beta <- Mean.beta/StdDev.beta

    print(data.frame(Mean.beta, StdDev.beta, Z.beta))

    if (plots) {
        ## nRows <- ceiling(sqrt(nParameters))
        nRows <- nParameters
        ## nCols <- floor(sqrt(nParamerts))
        nCols <- 2
        oldPar <- par(mfrow = c(nRows, nCols))
        nms <- names(beta.df)

        for (i in 1:nParameters) {
            plot(ts(beta.df[, i]), main = paste("Time series plot of", nms[i]), ylab = nms[i])
            plot(acf(beta.df[, i], plot = FALSE), main = paste("Autocorrelation plot of", nms[i]))
        }

        par(oldPar)
    }

    invisible(list(beta = beta.df, mleMean = mleMean, mleVar = mleVar))
}
