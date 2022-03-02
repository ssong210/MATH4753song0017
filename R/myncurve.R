#
#' @title myrcurve()
#'
#' @description Creates and plots random sample trials of binary distributions.
#'
#' @param iter number of iterations, default value 100
#' @param n sample size, default value 10
#' @param p probability, default value 0.5
#'
#' @return A barplot which tabulates each experiment's results
#' @export
#'
#' @examples mybin(30,10,0.7)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-4*sigma,a,length=1000)

  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(mu-4*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)

  prob
}
