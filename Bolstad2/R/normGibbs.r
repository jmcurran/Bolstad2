#' Draw a sample from a posterior distribution of data with an unknown mean and
#' variance using Gibbs sampling
#' 
#' normGibbs draws a Gibbs sample from the posterior distribution of the
#' parameters given the data fron normal distribution with unknown mean and
#' variance.  The prior for \eqn{\mu} given \eqn{var} is prior mean \eqn{m0}
#' and prior variance \eqn{var/n0} .  That means \eqn{n0} is the 'equivalent
#' sample size.' The prior distribution of the variance is \eqn{s0} times an
#' inverse chi-squared with \eqn{kappa0} degrees of freedom. The joint prior is
#' the product \eqn{g(var)g(mu|var)}.
#' 
#' 
#' @param y A vector containing the data
#' @param steps The number of iterations of Gibbs sampling to carry out
#' @param type Either 'ind' for sampling from an independent conjugate prior or
#' 'joint' for sampling from a joint conjugate prior. 'i' and 'j' can be used
#' as compact notation
#' @param \dots If type = 'ind' then the user can specify the prior for
#' \eqn{\mu} with a parameter priorMu which can either be a single number m0,
#' or m0 and n0. if m0 and n0 are not specified then m0 and n0 are 0 by
#' default. The user can also specify priorVar, which if given, must be a
#' vector with two elements s0 and kappa0. If s0 and kappa0 are not given then
#' they are zero by default. If type = 'joint' then priorMu must be a vector of
#' length two with elements m0 and sd0.  The user can also specify priorVar,
#' which if given, must be a vector with two elements s0 and kappa0. If s0 and
#' kappa0 are not given then they are zero by default.
#' @return A data frame containing three variables \tabular{rll}{ [1,] \tab mu
#' \tab a sample from the posterior distribution of the mean \cr [2,] \tab sig
#' \tab a sample from the posterior distribution of the standard deviation \cr
#' [3,] \tab mu \tab a sample from the posterior distribution of the variance =
#' sig^2 \cr }
#' @author James M. Curran
#' @examples
#' 
#' ## firstly generate some random data
#' mu = rnorm(1)
#' sigma = rgamma(1,5,1)
#' y = rnorm(100, mu, sigma)
#' 
#' ## A \eqn{N(10,3^2)} prior for \eqn{\mu} and a 25 times inverse chi-squared
#' ## with one degree of freedom prior for \eqn{\sigma^2}
#' MCMCSampleInd = normGibbs(y, steps = 5000, priorMu = c(10,3),
#'                            priorVar = c(25,1))
#' 
#' 
#' ## We can also use a joint conjugate prior for \eqn{\mu} and \eqn{\sigma^2}.
#' ## This will be a \emph{normal}\eqn{(m,\sigma^2/n_0)} prior for \eqn{\mu} given
#' ## the variance \eqn{\sigma^2}, and an \eqn{s0} times an \emph{inverse
#' ## chi-squared} prior for \eqn{\sigma^2}.
#' MCMCSampleJoint = normGibbs(y, steps = 5000, type = 'joint',
#'                              priorMu = c(10,3), priorVar = c(25,1))
#' 
#' ## Now plot the results
#' oldPar = par(mfrow=c(2,2))
#' 
#' plot(density(MCMCSampleInd$mu),xlab=expression(mu), main =
#' 'Independent')
#' abline(v=mu)
#' plot(density(MCMCSampleInd$sig),xlab=expression(sig), main =
#' 'Independent')
#' abline(v=sigma)
#' 
#' plot(density(MCMCSampleJoint$mu),xlab=expression(mu), main =
#' 'Joint')
#' abline(v=mu)
#' plot(density(MCMCSampleJoint$sig),xlab=expression(sig), main =
#' 'Joint')
#' abline(v=sigma)
#' 
#' 
#' @export normGibbs
normGibbs = function(y, steps = 1000, type = "ind", ...) {

    if (length(grep("[Ii]", type)) > 0) {
        type = "ind"
    } else if (length(grep("[Jj]", type)) > 0) {
        type = "joint"
    } else {
        stop("Type must be ind or joint")
    }

    dots = list(...)


    if (type == "ind") {

        ## The dots can carry priorMu which, if specified can be a single number m0 or a vector
        ## m0, n0

        m0 = 0
        n0 = 0
        muSpecified = FALSE

        if ("priorMu" %in% names(dots)) {
            if (length(dots$priorMu) == 2) {
                m0 = dots$priorMu[1]
                n0 = dots$priorMu[2]
                muSpecified = TRUE
            } else {
                m0 = dots$priorMu
                muSpecified = TRUE
            }
        }

        ## The dots can carry priorVar which, if specified must be a vector of length 2

        s0 = 0
        kappa0 = 0
        nObs = length(y)
        yBar = mean(y)
        SSy = sum((y - yBar)^2)

        varSpecified = FALSE

        if ("priorVar" %in% names(dots)) {
            if (length(dots$priorVar) != 2)
                stop("priorVar must have two elements, s0 and kappa0") else {
                s0 = dots$priorVar[1]
                kappa0 = dots$priorVar[2]
                varSpecified = TRUE
            }
        }

        v0 = ifelse(varSpecified, s0/rchisq(1, df = kappa0), SSy/(nObs - 1))
        kappa1 = kappa0 + nObs

        prec0 = 0
        mu0 = yBar

        if (muSpecified) {
            prec0 = n0/v0
            mu0 = m0 + rnorm(1) * sqrt(v0)
        } else {
            prec0 = 0
            m0 = 0
            mu0 = yBar
        }

        SSt = sum((y - mu0)^2)
        s1 = s0 + SSt

        Chi = rchisq(steps, df = kappa1)
        z = rnorm(steps)

        varSample = c(s1/Chi[1], rep(0, steps - 1))


        prec0 = ifelse(muSpecified, n0/varSample[1], 0)

        precData = nObs/varSample[1]
        prec1 = prec0 + precData
        v1 = 1/prec1
        m1 = m0 * prec0/prec1 + yBar * precData/prec1

        muSample = rep(0, steps)
        muSample[1] = z[1] * sqrt(v1) + m1

        for (i in 2:steps) {
            SSt = sum((y - muSample[i - 1])^2)
            s1 = s0 + SSt
            varSample[i] = s1/Chi[i]

            prec0 = ifelse(muSpecified, n0/varSample[i], 0)
            precData = nObs/varSample[i]
            prec1 = prec0 + precData
            v1 = 1/prec1
            std1 = sqrt(v1)
            m1 = m0 * prec0/prec1 + yBar * precData/prec1

            muSample[i] = z[i] * std1 + m1
        }

        sigmaSample = sqrt(varSample)

        oldPar = par(mfrow = c(2, 2))

        plot(ts(muSample), main = "Time series plot of mu", ylab = expression(mu))
        plot(ts(varSample), main = "Time series plot of var", ylab = expression(sigma^2))
        plot(ts(sigmaSample), main = "Time series plot of sigma", ylab = expression(sigma))
        hist(muSample, main = "Histogram of mu", prob = TRUE)
        mx = mean(muSample)
        sx = sd(muSample)
        bds = mx + c(-5, 5) * sx
        xValues = seq(bds[1], bds[2], length = 200)
        yValues = dnorm(xValues, mx, sx)
        lines(xValues, yValues)

        par(oldPar)

        results.df = data.frame(mu = muSample, sig = sigmaSample, var = varSample)

        describe(results.df)
        invisible(results.df)
    } else {
        ## type = 'joint'

        ## The dots can carry priorMu which, if specified can be a single number m0 or a vector
        ## m0, n0

        m0 = 0
        n0 = 0
        v0 = 0

        muSpecified = FALSE

        if ("priorMu" %in% names(dots)) {
            if (length(dots$priorMu) == 2) {
                m0 = dots$priorMu[1]
                n0 = dots$priorMu[2]
                muSpecified = TRUE
            } else {
                stop("priorMu must contain a mean and an effective sample size")
            }
        }

        ## The dots can carry priorVar which, if specified must be a vector of length 2

        s0 = 0
        kappa0 = 0
        varSpecified = FALSE

        v0 = 0

        if ("priorVar" %in% names(dots)) {
            if (length(dots$priorVar) != 2)
                stop("priorVar must have two elements, s0 and kappa0") else {
                s0 = dots$priorVar[1]
                kappa0 = dots$priorVar[2]
                varSpecified = TRUE
            }
        }

        nObs = length(y)
        yBar = mean(y)
        SSy = sum((y - yBar)^2)

        v0 = ifelse(varSpecified, s0/rchisq(1, df = kappa0), SSy/(nObs - 1))

        kappa1 = kappa0 + nObs

        prec0 = 0
        mu0 = yBar

        muSample = rep(0, steps)

        if (muSpecified) {
            prec0 = n0/v0
            mu0 = m0 + rnorm(1) * sqrt(v0)
        } else {
            m0 = 0
        }


        SSt = sum((y - mu0)^2)
        s1 = s0 + SSt

        Chi = rchisq(steps, df = kappa1)

        varSample = c(s1/Chi[1], rep(0, steps - 1))
        Z = rnorm(steps)

        prec0 = ifelse(muSpecified, n0/varSample[1], 0)
        precData = nObs/varSample[1]
        prec1 = prec0 + precData
        v1 = 1/prec1
        m1 = prec0/prec1 * m0 + precData/prec1 * yBar

        muSample[1] = Z[1] * sqrt(v1) + m1

        for (i in 2:steps) {
            SSt = sum((y - muSample[i - 1])^2)
            s1 = s0 + SSt
            varSample[i] = s1/Chi[i]

            prec0 = ifelse(muSpecified, n0/varSample[i], 0)
            precData = nObs/varSample[i]
            prec1 = prec0 + precData
            v1 = 1/prec1
            m1 = prec0/prec1 * m0 + precData/prec1 * yBar

            muSample[i] = Z[i] * sqrt(v1) + m1
        }

        sigmaSample = sqrt(varSample)

        oldPar = par(mfrow = c(2, 2))

        plot(ts(muSample), main = "Time series plot of mu", ylab = expression(mu))
        plot(ts(varSample), main = "Time series plot of var", ylab = expression(sigma^2))
        plot(ts(sigmaSample), main = "Time series plot of sigma", ylab = expression(sigma))
        hist(muSample, main = "Histogram of mu", prob = TRUE)
        mx = mean(muSample)
        sx = sd(muSample)
        bds = mx + c(-5, 5) * sx
        xValues = seq(bds[1], bds[2], length = 200)
        yValues = dnorm(xValues, mx, sx)
        lines(xValues, yValues)

        par(oldPar)

        results.df = data.frame(mu = muSample, sig = sigmaSample, var = varSample)

        describe(results.df)
        invisible(results.df)
    }
}





