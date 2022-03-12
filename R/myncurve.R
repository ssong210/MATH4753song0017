#
#' @title myncurve()
#'
#' @description Plots lower tail curve area of given normal distribution and given value
#'
#' @usage myncurve(mu, sigma, a)
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a value for lower tail
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @export
#'
#' @examples \dontrun{myncurve(30,10,0.7)}
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-4*sigma,a,length=1000)

  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(mu-4*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)

  prob
}
