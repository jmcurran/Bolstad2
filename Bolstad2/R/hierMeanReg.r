#' Hierarchical Normal Means Regression Model
#' 
#' fits a hierarchical normal model of the form \eqn{E[y_{ij}] = \mu_{j} +
#' \beta_{1}x_{i1}+\dots+\beta_{p}x_{ip}}
#' 
#' 
#' @param design a list with elements y = response vector, group = grouping
#' vector, x = matrix of covariates or NULL if there are no covariates
#' @param priorTau a list with elements tau0 and v0
#' @param priorPsi a list with elements psi0 and eta0
#' @param priorVar a list with elements s0 and kappa0
#' @param priorBeta a list with elements b0 and bMat or NULL if x is NULL
#' @param steps the number of Gibbs sampling steps to take
#' @param startValue a list with possible elements tau, psi, mu, sigmasq and
#' beta. tau, psi and sigmasq must all be scalars. mu and beta must be vectors
#' with as many elements as there are groups and covariates respectively
#' @param randomSeed a random seed for the random number generator
#' @return A data frame with variables: \item{tau}{Samples from the posterior
#' distribution of tau} \item{psi}{Samples from the posterior distribution of
#' psi} \item{mu}{Samples from the posterior distribution of mu}
#' \item{beta}{Samples from the posterior distribution of beta if there are any
#' covariates} \item{sigmaSq}{Samples from the posterior distribution of
#' \eqn{\sigma^2}} \item{sigma}{Samples from the posterior distribution of
#' sigma}
#' @examples
#' 
#' priorTau <- list(tau0 = 0, v0 = 1000)
#' priorPsi <- list(psi0 = 500, eta0 = 1)
#' priorVar <- list(s0 = 500, kappa0 = 1)
#' priorBeta <- list(b0 = c(0,0), bMat = matrix(c(1000,100,100,1000), nc = 2))
#' 
#' data(hiermeanRegTest.df)
#' data.df <- hiermeanRegTest.df
#' design <- list(y = data.df$y, group = data.df$group,
#'                x = as.matrix(data.df[,3:4]))
#' r<-hierMeanReg(design, priorTau, priorPsi, priorVar, priorBeta)
#' 
#' oldPar <- par(mfrow = c(3,3))
#' plot(density(r$tau))
#' plot(density(r$psi))
#' plot(density(r$mu.1))
#' plot(density(r$mu.2))
#' plot(density(r$mu.3))
#' plot(density(r$beta.1))
#' plot(density(r$beta.2))
#' plot(density(r$sigmaSq))
#' par(oldPar)
#' 
#' ## example with no covariates
#' priorTau <- list(tau0 = 0, v0 = 1000)
#' priorPsi <- list(psi0 = 500, eta0 = 1)
#' priorVar <- list(s0 = 500, kappa0 = 1)
#' 
#' data(hiermeanRegTest.df)
#' data.df <- hiermeanRegTest.df
#' design <- list(y = data.df$y, group = data.df$group, x = NULL)
#' r<-hierMeanReg(design, priorTau, priorPsi, priorVar)
#' 
#' oldPar <- par(mfrow = c(3,2))
#' plot(density(r$tau))
#' plot(density(r$psi))
#' plot(density(r$mu.1))
#' plot(density(r$mu.2))
#' plot(density(r$mu.3))
#' plot(density(r$sigmaSq))
#' par(oldPar)
#' 
#' 
#' @export hierMeanReg
hierMeanReg<-function(design, priorTau, priorPsi, priorVar, priorBeta = NULL,
                      steps = 1000, startValue = NULL, randomSeed = NULL){
    ## design is expected to be a list with elements,
    ## y = response vector,
    ## group  = grouping vector,
    ## x = matrix of covariates or null

    ## priorTau is a list with elements tau0 and v0
    ## priorPsi is a list with elements psi0 and eta0
    ## priorVar is a list with elements s0 and kappa0
    ## priorBeta is a list with elements b0 and bMat or null if x is null
    ## startValue is a list with possible elements tau, psi, mu, sigma and beta

    if(!is.null(randomSeed))
        set.seed(randomSeed)

    names(design)<-tolower(names(design))

    if(any(!(names(design) %in% c("y","group","x")))){
        stop("design must have elements y, group and x")
    }

    names(priorTau)<-tolower(names(priorTau))
    if(any(!(names(priorTau) %in% c("tau0","v0")))){
        stop("priorTau must have elements tau0 and v0")
    }

    names(priorPsi)<-tolower(names(priorPsi))
    if(any(!(names(priorPsi) %in% c("psi0","eta0")))){
        stop("priorPsi must have elements psi0 and eta0")
    }

    names(priorVar)<-tolower(names(priorVar))
    if(any(!(names(priorVar) %in% c("s0","kappa0")))){
        stop("priorVar must have elements s0 and kappa0")
    }

    if(!is.null(startValue)){
        names(startValue)<-tolower(names(startValue))
        if(any(!(names(startValue) %in% c("tau","psi","mu","sigmasq",
                                          "beta")))){
            stop("startValue can only have elements tau, psi, mu, sigmasq, beta")
        }
    }

    bReg <- TRUE

    if(is.null(design$x)){
        nCovariates <- 0
        bReg <- FALSE
    }else{
        if(any(!(names(priorBeta) %in% c("b0","bMat")))){
            stop("priorBeta must have elements b0 and bMat")
        }
        nCovariates <- ncol(design$x)

        if(length(priorBeta$b0)!=nCovariates)
            stop("b0 must have as many elements as there are covariates")

        if(ncol(priorBeta$bMat)!=nrow(priorBeta$bMat))
            stop("bMat must be a square matrix")

        if(ncol(priorBeta$bMat)!=nCovariates)
            stop("bMat must have as many rows(columns) as there are covariates")
    }

    nObs <- length(design$y)

    design$group <- factor(design$group)
    nGroups <- length(levels(design$group))


    kappa1 <- priorVar$kappa0 + nObs
    eta1 <- priorPsi$eta0 + nGroups

    y <- design$y
    group <- design$group

    X <- Xt <- XtX <- bMat2 <- prec0 <- precObs <- prec1 <- NULL
    betaSample <- zBeta <- NULL

    if(bReg){
        X <- design$x
        Xt <- t(X)
        XtX <- Xt %*% X

        prec0 <- solve(priorBeta$bMat)
        bMat2 <- prec0 %*% priorBeta$b0
        betaSample <- matrix(0, nc = nCovariates, nr = steps)

        L <- t(chol(priorBeta$bMat))
        w4 <- rnorm(nCovariates)

        if(!is.null(startValue) & ("beta" %in% names(startValue))){
            betaSample[1,] <- startValue$beta
        }else{
            betaSample[1,] <- L%*%w4
        }
        zBeta <- matrix(rnorm(nCovariates*steps), nc = nCovariates)
    }

    tauSample <- rep(0, steps)
    psiSample <- rep(0, steps)
    muSample <- matrix(0, nc = nGroups, nr = steps)

    sigmaSqSample <- rep(0, steps)

    if(!is.null(startValue) & ("tau" %in% names(startValue))){
        tauSample[1] <- startValue$tau
    }else{
        tauSample[1] <- priorTau$tau0 + rnorm(1)*sqrt(priorTau$v0)
    }

    if(!is.null(startValue) & ("psi" %in% names(startValue))){
        psiSample[1] <- startValue$psi
    }else{
        psiSample[1] <- priorPsi$eta0/rchisq(1,df = priorPsi$eta0)
    }

    if(!is.null(startValue) & ("mu" %in% names(startValue))){
        muSample[1,] <- startValue$mu
    }else{
        muSample[1, ] <- tauSample[1] + rnorm(3)*sqrt(psiSample[1])
    }

    if(!is.null(startValue) & ("sigmasq" %in% names(startValue))){
        sigmaSqSample[1] <- startValue$sigmasq
    }else{
        sigmaSqSample[1] <- priorVar$kappa0 /rchisq(1, df = priorVar$kappa0)
    }

    nj <- sapply(split(y, group),length)

    ## pre-generate arrays of r.v's

    zTau <- rnorm(steps)
    chiPsi <- rchisq(steps, df = eta1)
    zMu <- matrix(rnorm(nGroups*steps), nc = nGroups)
    chiSigmaSq <- rchisq(steps, df = kappa1)

    mu1 <- rep(0, nGroups)

    for(n in 2:steps){
        muSum <- sum(muSample[n-1,])
        muBar <- muSum/nGroups
        v1 <- priorTau$v0*psiSample[n-1]/(psiSample[n-1]+nGroups*priorTau$v0)
        tau1 <- v1*(priorTau$tau0/priorTau$v0 + nGroups*muBar/psiSample[n-1])
        tauSample[n] <- tau1 + zTau[n]*sqrt(v1)

        SSq <- sum((muSample[n-1,]-tauSample[n])^2)
        psi1 <- priorPsi$psi0 + SSq
        psiSample[n] <- psi1/chiPsi[n]

        for(j in 1:nGroups){
            varMu <- (sigmaSqSample[n-1]*psiSample[n-1])/(sigmaSqSample[n-1] +
                                                          nj[j]*psiSample[n-1])
            xBeta <- 0
            if(bReg)
                xBeta <- X[group==j,]%*%betaSample[n-1,]

            zBar <-  mean(y[group==j] - xBeta)
            muBar <- varMu*(tauSample[n]/psiSample[n-1] +
                            nj[j]*zBar/sigmaSqSample[n-1])
            muSample[n,j] <- muBar + zMu[n,j]*sqrt(varMu)
            mu1[j] <- muSample[n,j]
        }
        yMu <- y - mu1[group]
        xBeta <- 0

        if(bReg){
            precObs <- XtX / sigmaSqSample[n-1]
            prec1 <- prec0 + precObs
            bMat1 <- solve(prec1)

            bLS <- coef(lm(yMu~-1+X))

            b1 <- bMat1%*%bMat2 + bMat1%*%(precObs%*%bLS)

            L <- t(chol(bMat1))
            betaSample[n,] <- L %*% zBeta[n,] + b1
            xBeta <- X %*% betaSample[n,]
        }

        SSw <- sum((yMu-xBeta)^2)
        s1 <- priorVar$s0 + SSw
        sigmaSqSample[n]<-s1/chiSigmaSq[n]
    }

    sigmaSample <- sqrt(sigmaSqSample)

    results.df<-NULL

    if(bReg){
       results.df <- data.frame(tau = tauSample, psi = psiSample,
                                mu = muSample, beta = betaSample,
                                sigmaSq = sigmaSqSample, sigma = sigmaSample)
    }else{
        results.df <- data.frame(tau = tauSample, psi = psiSample,
                                 mu = muSample, sigmaSq = sigmaSqSample,
                                 sigma = sigmaSample)
    }
    describe(results.df)

    invisible(results.df)
}

