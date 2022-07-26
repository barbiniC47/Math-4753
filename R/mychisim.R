
#' Title
#'
#' @param n1
#' @param sigma1
#' @param mean1
#' @param iter
#' @param ymax
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
#'
#'
mychisim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1, x=20, y=0.1){
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)#Grabs a random sample from the normal distribution (which is built from n1*iter observations, mu, and sigma)

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1. Want to fill the constructed matrix going by rows.

  ssq1=apply(data1.mat,2,var) # ssq1 is s squared

  w=(n1-1)*ssq1/sigma1^2      #this is your constructed chi-sq stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Adding annotations to the histogram
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Blue",lwd=3)
  curve(dchisq(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # This is your theoretical curve. Charted in red
  title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2)) #Adds mathematical notation to your title
  legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Adds a key/legend to che chart
  return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # What the function should return.
}
