myGrad <- function(t, y, parms=NULL) {
	dy1 <- -((y[1]/(ee+y[1]))*(aa/(bb+cc*y[2]))*y[2]) + rr*y[2] + (beta*lambda*y[2]^2/(mu^2+y[2]^2))*y[3] + gamma*qq*y[3] + kk*(N0-y[1])
	dy2 <- (y[1]/(ee+y[1]))*(aa/(bb+cc*y[2]))*y[2] - rr*y[2] - (lambda*y[2]^2/(mu^2+y[2]^2))*y[3] - (ss+kk)*y[2]
	dy3 <- (alpha*lambda*y[2]^2/(mu^2+y[2]^2))*y[3] - qq*y[3]
	return(list(c(dy1,dy2,dy3), NULL)) }

states <- c("N", "P", "Z")
