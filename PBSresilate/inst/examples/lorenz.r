myGrad <- function(t, y, parms=NULL) {
	dy1 <- sigma*(y[2]-y[1]) 
	dy2 <- y[1]*(tau-y[3]) - y[2]
	dy3 <- y[1]*y[2] - rho*y[3]
	return(list(c(dy1,dy2,dy3), NULL)) }

states <- c("x", "y", "z")
