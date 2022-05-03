#
#' @title mycltu: Sample and graph from a uniform distribution
#'
#' @description Creates a random sample from a specified uniform distribution
#' and makes a histogram from sample means.
#'
#' @usage mycltu(n,iter,a,b)
#'
#' @param n sample size of each experiment
#' @param iter number of iterations of experiment
#' @param a lower bound of uniform distribution
#' @param b upper bound of uniform distribution
#'
#' @importFrom stats density dunif runif
#' @importFrom graphics hist lines
#'
#' @details This function creates a random sample from a uniform distribution
#' from parameters a and b, each sample being size n, and there are iter iterations
#' of this experiment. The means of each iteration's samples will be returned
#' as a list and a histogram of the means is made.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mycltu(n=20,iter=100000)
#' }
#'
### CLT uniform
## my Central Limit Function
## Notice that I have assigned default values which can be changed when the function is called
mycltu=function(n,iter,a=0,b=10){
  ## r-random sample from the uniform
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)

}
