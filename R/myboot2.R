#
#' @title myboot2: Bootstraps a given sample
#'
#' @description Creates a bootstrap interval of given statistic and plots a histogram of the vector of that statistic.
#'
#' @usage myboot2(iter, x, fun, alpha, cx, ...)
#'
#' @param iter how many times to run the sample of the given sample
#' @param x sample to bootstrap
#' @param fun function for the statistic to take from sample
#' @param alpha defines the quantile that is discarded from confidence interval
#' @param cx scales text and symbols in graph
#' @param ... additional parameters for hist() function
#'
#' @importFrom graphics abline segments text
#' @importFrom stats quantile
#'
#' @details This function makes iter samples from given sample x and finds the
#' statistic (function for this statistic supplied as parameter) for each of the
#' samples created from x. A list of the confidence interval, the function, the
#' given sample, and the created vector of statistics is returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sam=rnorm(n=20,mean=10,sd=4)
#' myboot2(iter=10000,x=sam,fun="mean",alpha=0.20)
#' }
#'
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE) #A
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# B: Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x,xstat=xstat))# Some output to use if necessary
}
