myGrad <- function(t, y, parms=NULL) {
	dy1 <- rB*y[1]*(1-(y[1]/(Kpr*y[2]))*((TE^2+y[3]^2)/y[3]^2)) - beta*y[1]^2/((alphapr*y[2])^2+y[1]^2)
	dy2 <- rS*y[2]*(1-((y[2]*KE)/(y[3]*KS)))
	dy3 <- rE*y[3]*(1-y[3]/KE) - Ppr*(y[1]/y[2])*(y[3]^2/(TE^2+y[3]^2))
	return(list(c(dy1,dy2,dy3), NULL)) }

states <- c("B", "S", "E")
