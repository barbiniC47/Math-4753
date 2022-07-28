#' ntickets function
#'
#' @param N = Number of Plane Seats
#' @param gamma = Probability of Overbooking
#' @param p = Probability of Not Showing
#'
#' @return mylist, plotd, plotc
#' @export
#'
#' @examples ntickets(N=200, gamma=0.02, p=0.95)
#' @importFrom graphics 'lines' 'abline'
#' @importFrom stats 'qbinom' 'pbinom' 'rnorm' 'pnorm'



ntickets <- function(N, gamma, p){


### STARTING WITH THE FUNCTIONS ###

  n <- seq(N, floor(N+(N/10)), by=1)# Here's n, discrete.
  nd <- qbinom(p=1-gamma, size=n, prob=p)
  #Finding possible values of n.
  nn <- seq(N, floor(N+(N/10)), length = 10000) #here's n, but as a normal approx.
  #Finding possible values of nn.

### NOW OBJECTIVE FUNCTIONS ###

  yd <- 1 - gamma - pbinom(q=N, size=n, prob=p) #discrete
  fd <- which.min(abs(yd ))
  #Finding the zeroes with the objective function
  yc <- 1 - gamma - pnorm(q=N+0.05, mean=nn*p, sd=(sqrt(nn*p*(1-p)))) #Normal approx.
  fc <- which.min(abs(yc))
  #Finding the zeroes with the objective function

### NOW PLOTTING ###

#Binomial Plot
  plotd <- plot(x=n, y=yd, main = paste("Objective vs. n to find optimal number of tickets to sell, Binomial\n", "Number of Seats:" , N, "  Number of Tickets to Sell:", n[fd]),  xlab = "Number of Tickets, n", ylab = "Objective", pch=20 )
  lines(x=n, y=yd, col = "Blue")
  abline(v=n[fd], h=0, col="Red", lty=1, lwd=2)
  #v and h are the coordinates of the minimum/"zero" point. Lines drawn to reflect that.

#Normal Approximation Plot
  plotc <- plot(x=nn, y=yc, main = paste("Objective vs. n to find optimal number of tickets to sell, Normal Approximation\n", "Number of Seats:" , N, "  Number of Tickets to Sell:", nn[fc]), xlab = "Number of Tickets, n", ylab = "Objective", pch = 20, type="n" )
  lines(x=nn, y=yc, col = "Black")
  abline(v=nn[fc], h=0, col = "Red", lty=1, lwd=2)
  #v and h are the coordinates of the minimum/"zero" point. Lines drawn to reflect that

### NOW GETTING THE LIST OF VALUES ###

  mylist <- list(N=N, gamma=gamma, p=p, nd=n[fd], nc=nn[fc], ntickets=paste("N=",N, "gamma=", gamma, "p=", p, fun="Number of Tickets"))
  return(c(mylist, plotd, plotc))
}



#   ── R CMD check results ───────────────────────────── summer4753bar 0.1.0 ────
#   Duration: 15.1s

#   ❯ checking DESCRIPTION meta-information ... NOTE
#   License components which are templates and need '+ file LICENSE':
#   MIT License

#   ❯ checking top-level files ... NOTE
#   Non-standard file/directory found at top level:
#   'Rplots.pdf'

#   0 errors ✔ | 0 warnings ✔ | 2 notes ✖

#   R CMD check succeeded

