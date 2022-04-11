#' Give simple descriptive statistics for a matrix or a data frame
#' 
#' This function is designed to emulate the Minitab function DESCRIBE. It gives
#' simple descriptive statistics for a data frame
#' 
#' 
#' @param x A matrix or data.frame with numeric entries. Different variables
#' are represented by columns.
#' @param varNames A vector of variable names for each of the columns
#' @return A data.frame containing the following elements: \item{N}{The number
#' of observations for each variable} \item{mean}{The sample mean for each
#' variable} \item{stdev}{The sample standard deviation} \item{sterr}{The
#' standard error of the mean} \item{min}{The minimum} \item{q1}{The lower
#' quartile} \item{med}{The median} \item{q3}{The upper quartile}
#' \item{max}{The maximum}
#' @examples
#' 
#' data(poissonTest.df)
#' describe(poissonTest.df)
#' 
#' @export describe
describe<-function(x, varNames = NULL){
    ## Mimics Minitab's desc function

    nameX<-deparse(substitute(x))

    if(is.matrix(x)){
        x<-data.frame(x)
        if(is.null(varNames))
            varNames<-paste(nameX,0:(ncol(x)-1),sep="")
        names(x)<-varNames
    }

    nx<-sapply(x,length)
    mx<-sapply(x,mean)
    sx<-sapply(x,sd)
    SEx<-sapply(x,sd)/sqrt(nx)
    minx<-sapply(x,min)
    maxx<-sapply(x,max)
    q1x<- sapply(x,quantile,prob=0.25)
    medx<-sapply(x,median)
    q3x<-sapply(x,quantile,prob=0.75)

    stats.df<-data.frame(N=nx, mean = mx, stdev = sx, sterr = SEx,
                        min = minx, q1 = q1x, med = medx, q3 = q3x, max = maxx)

    nVars <- ncol(x)

    print(stats.df)

    invisible(stats.df)
}
