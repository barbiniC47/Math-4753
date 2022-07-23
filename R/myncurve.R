
myncurve <= function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim=c(mu-3*sigma, mu + 3*sigma), col = "black")

  xcurve = seq(-Inf, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)

  polygon(c(-Inf, xcurve, a), c(0, ycurve, 0), col = "cyan")

  prob = round(pnorm(a, mu, sigma), 4)
  return(myncurve)

}
