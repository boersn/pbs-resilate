# Original code from Sanae

#*********************************************************************************************
#R code for displaying the 2-body problem
#*********************************************************************************************

lawOfGravitation <- function(t, State, Pars) {
  with(as.list(c(State, Pars)), {

  #This part doesn't seem to be working
  dx1 <- u1
  dy1 <- v1
  dx2 <- u2
  dy2 <- v2
  
  #This part seems to be working
  d_sqrd <- (x1 - x2)^2 + (y1 - y2)^2
  xScale <- abs(x1-x2)/sqrt(d_sqrd)
  yScale <- abs(y1-y2)/sqrt(d_sqrd)
  du1 <- xScale*G*m2/d_sqrd
  dv1 <- yScale*G*m2/d_sqrd  
  du2 <- xScale*G*m1/d_sqrd
  dv2 <- yScale*G*m1/d_sqrd

  return(c(dx1, dy1, dx2, dy2, du1, dv1, du2, dv2))
  })
}

calcTraj <- function() {
   getWinVal(scope="L")

   initialValues<- c(x1=wx1, y1=wy1, x2=wx2, y2=wy2, u1=wu1, v1=wv1, u2=wu2, v2=wv2)
                      
   pars <- c(G=1, m1=m1, m2=m2)
   times <- seq(0, 100, by=1)

#   out <- ode(func = lawOfGravitation, y=initialValues, parms=pars, times=times)
       
   out <- dde(y=initialValues, func=lawOfGravitation, times=times, parms=pars)
   out <- as.data.frame(out)

   colours <- colorRampPalette(c("red", "orange", "yellow", 
            "green", "blue"), space="Lab")(100)
   pairs(out, col=colours)
  }

# Load PBS Modelling and initialize the GUI

require(PBSmodelling); require(ddesolve); createWin("2body.txt")
