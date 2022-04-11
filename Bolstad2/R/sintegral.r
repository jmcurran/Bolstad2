#' Numerical integration using Simpson's Rule
#' 
#' Takes a vector of \eqn{x} values and a corresponding set of postive
#' \eqn{f(x)=y} values and evaluates the area under the curve: \deqn{
#' \int{f(x)dx} }.
#' 
#' 
#' @param x a sequence of \eqn{x} values.
#' @param fx the value of the function to be integrated at \eqn{x}.
#' @param n.pts the number of points to be used in the integration.
#' @return returns a list with the following elements \item{x}{the x-values at
#' which the integral has been evaluated} \item{y}{the cummulative integral}
#' \item{int}{the value of the integral over the whole range}
#' @keywords misc
#' @examples
#' 
#' ## integrate the normal density from -3 to 3
#' x=seq(-3,3,length=100)
#' fx=dnorm(x)
#' estimate=sintegral(x,fx)$int
#' true.val=diff(pnorm(c(-3,3)))
#' cat(paste('Absolute error :',round(abs(estimate-true.val),7),'\n'))
#' cat(paste('Relative percentage error :', 100*round((abs(estimate-true.val)/true.val),6),'%\n'))
#' 
#' @export sintegral
sintegral = function(x, fx, n.pts = 256) {
    ## numerically integrates fx over x using Simpsons rule x - a sequence of x values fx - the
    ## value of the function to be integrated at x n.pts - the number of points to be used in
    ## the integration


    n.x = length(x)

    if (n.x != length(fx))
        stop("Unequal input vector lengths")

    if (n.pts < 64)
        n.pts = 64

    if (length(x) > n.pts)
        n.pts = length(x)

    ## use linear approximation to get equally spaced x values


    ap = approx(x, fx, n = 2 * n.pts + 1)

    h = diff(ap$x)[1]

    integral = h * (ap$y[2 * (1:n.pts) - 1] + 4 * ap$y[2 * (1:n.pts)] + ap$y[2 * (1:n.pts) + 1])/3

    (list(x = ap$x[2 * (1:n.pts)], y = cumsum(integral), int = sum(integral)))
}
