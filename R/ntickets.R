#' Number of Tickets
#'
#' @param N number of seats
#' @param gamma probability of plane being overbooked
#' @param p probability of someone showing up
#'
#' @returns A list containing nd, nc, p, and gamma. Creates two graphs displaying nd - using discrete distribution, and nc - using normal approximation
#' @export
#'
#' @importFrom graphics plot abline curve
#' @importFrom stats pbinom pnorm optimize
#'
#' @examples ntickets(400, 0.02, 0.95)
ntickets = function(N, gamma, p) {

  # Number of seats in the plane
  seats <- N

  # Probability that the plane will NOT be overbooked
  target_seats <- 1 - gamma

  #finding the number of tickets to sell using a discrete distribution
  fnd <- function(nd) {
    target_seats - pbinom(N, nd, p)
  }

  nd <- N - 1 + which.min(abs(fnd(N:floor(N*1.1))))
  nd

  #finding the number of tickets to sell using a normal approximation
  q <- 1 - p
  fn <- function(nc) {
    prob_nc <- pnorm(N + 0.5, mean = nc * p, sd = sqrt(nc * p * q))
    abs(prob_nc - target_seats)
  }
  #use optimize to find minimum nc in the interval (N, N + 20)
  nc <- optimize(fn, interval = c(N, N + 20))$minimum



  #print list for nd, nc, N, p, and gamma
  list = list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)
  print(list)

  #create  plot of "objective function vs n" for discrete
  plot(N:floor(N*1.1), fnd(N:floor(N*1.1)), lwd = 2,
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective vs n to find optimal tickets sold (", nd, ") \ngamma = ", gamma, " N = ", N, " discrete", sep=""),
       pch = 16,
       type = "b",
       cex = .5)
  abline(v=nd, col = "red", lwd=2, h=0)

  #create plot of "objective function vs n" for continuous
  plotfn <- function(nc) {
    pnc <- 1-gamma-pnorm(N + 0.5, mean = nc * p, sd = sqrt(nc * p * q))

  }
  curve(plotfn,
        xlim = c(N,N*1.1),
        ylim=c(0,1),
        col = "black",
        lwd = 2, xlab = "n",
        ylab = "Objective",
        main = paste("Objective vs n to find optimal tickets sold (", nc, ") \ngamma = ", gamma, " N = ", N, " continuous", sep=""))
  abline(v=nc, col="blue", lwd=2, h=0)
}




