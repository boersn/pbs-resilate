# Original code from Sanae Rosen
# Modified by Jon Schnute

#*********************************************************************************************
#R code for displaying the 2-body problem
#*********************************************************************************************

calcGrad <- function(t, y, parms) { # gradient from Newton's law
  # unpack the states and parms
  x1 <- y[1]; y1 <- y[2]; x2 <- y[3]; y2 <- y[4];
  u1 <- y[5]; v1 <- y[6]; u2 <- y[7]; v2 <- y[8];
  G <- parms[1]; m1 <- parms[2]; m2 <- parms[3];

  # the law of gravity (I hope - JTS)
  r2 <- (x1 - x2)^2 + (y1 - y2)^2; r3 <- r2^(3/2); # square and cube of distance
  r <- c(x2-x1,y2-y1);                            # displacement vector from m1 to m2
  acc1 <- G*m2*r/r3; acc2 <- -G*m1*r/r3;          # acceleration vectors
  
  # Calculate the gradient
  dx1 <- u1; dy1 <- v1; dx2 <- u2; dy2 <- v2;
  du1 <- acc1[1]; dv1 <- acc1[2]; du2 <- acc2[1]; dv2 <- acc2[2];
  
  return(c(dx1, dy1, dx2, dy2, du1, dv1, du2, dv2)); };

calcTraj <- function() {
  getWinVal(scope="L");
  y0 <- c(x1=wx1, y1=wy1, x2=wx2, y2=wy2, u1=wu1, v1=wv1, u2=wu2, v2=wv2);
  pars <- c(G=1, m1=m1, m2=m2);
  times <- seq(0, nt, by=1);
  out <- dde(y=y0, func=calcGrad, times=times, parms=pars);
  out <- as.data.frame(out); return(out); };

plotPairs <- function() {
  out <- calcTraj(); getWinVal(scope="L"); # we need nt
  colours <- colorRampPalette(c("red", "orange", "yellow", 
            "green", "blue"), space="Lab")(nt+1);
   pairs(out, col=colours);
   invisible(out); };

plotTraj <- function() {
  out <- calcTraj(); x1 <- out[,1]; y1 <- out[,2]; x2 <- out[,3]; y2 <- out[,4];
  xrng <- range(c(x1,x2)); yrng <- range(c(y1,y2));
  plot(x1,y1,xlim=xrng,ylim=yrng,col="red");
  points(x2,y2,col="blue"); invisible(out); };

# Load PBS Modelling and initialize the GUI

require(PBSmodelling); require(ddesolve); createWin("2body.txt")
