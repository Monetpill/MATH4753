

#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a y value
#'
#' @returns the curve and the shaded area between the curve and x-axis from -infinity to x=a, and the area
#' @export
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @examples myncurve(mu = 0, sigma = 1, a = 2)
myncurve = function(mu = 0, sigma = 1, a){

  x <- NULL # at start of function

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), xlab = "Normal Distribution Curve", ylab = "Density", main = paste("Mean =", mu, "," , "SD =", sigma))


  xnorm = seq(mu-3*sigma, a, length = 1000)
  ynorm = dnorm(xnorm, mu, sigma)
  polygon(c(mu-3*sigma, xnorm, a), c(0, ynorm, 0), col = "pink")

  prob = pnorm(a, mu, sigma)

  text(x=a, y = 0.03, paste("Area =", round(prob,4), sep = ""), pos = 4)
  return(list(mu = mu, sigma = sigma, prob = prob))

}
