myGrad <- function(t, y, parms=NULL) {
	f1=function(x) { (a1*x)/(1+b1*x) }
	f2=function(x) { (a2*x)/(1+b2*x) }
	dy1 = y[1]*(1-y[1]) - f1(y[1])*y[2]
	dy2 = f1(y[1])*y[2] - f2(y[2])*y[3] - d1*y[2]
	dy3 = f2(y[2])*y[3] - d2*y[3]
	return(list(c(dy1,dy2,dy3), NULL)) }

states <- c("spX","spY","spZ")