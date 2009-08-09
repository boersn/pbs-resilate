# Original code from Sanae Rosen
# Modified by Jon Schnute

#*********************************************************************************************
#R code for displaying the 2-body problem
#*********************************************************************************************
#TODO:
#make trajectory time-dependent
#what happens if they hit the same point? they stop moving
#

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

  #center the system at (0, 0)
  if(CM) {
    newCoords <- centerSys(wx1, wy1, m1, wx2, wy2, m2);
    wx1 <- newCoords[1]; wy1 <- newCoords[2]; wx2 <- newCoords[3]; wy2 <- newCoords[4];
    };

  #center the velocity so the system has a net momentum of 0
  if(noMomentum) {
    newVels <- centerSys(wu1, wv1, m1, wu2, wv2, m2)
    wu1 <- newVels[1]; wv1 <- newVels[2]; wu2 <- newVels[3]; wv2 <- newVels[4];
    };

  y0 <- c(x1=wx1, y1=wy1, x2=wx2, y2=wy2, u1=wu1, v1=wv1, u2=wu2, v2=wv2);
  pars <- c(G=G, m1=m1, m2=m2);
  times <- seq(0, t, by=t/nt);
  out <- dde(y=y0, func=calcGrad, times=times, parms=pars);
  out <- as.data.frame(out); return(out); };

#takes in the coordinates and masses of the points, outputs new coordinates of the points so that the center of mass remains at (0, 0)
#also applicable for centering the velocity at 0
#tested: works correctly
centerSys <- function(a1, b1, m1, a2, b2, m2) { 
  aNet <- (a1*m1 + a2*m2)/(m1+m2);
  bNet <- (b1*m1 + b2*m2)/(m1+m2);
  a1 <- a1 - aNet;
  a2 <- a2 - aNet;
  b1 <- b1 - bNet;
  b2 <- b2 - bNet;
  return(c(a1, b1, a2, b2))
  };


plotPairs <- function() {
  out <- calcTraj(); getWinVal(scope="L"); # we need nt
  colours <- colorRampPalette(c("red", "orange", "yellow", 
            "green", "blue"), space="Lab")(nt+1);
   pairs(out, col=colours);
   invisible(out); };

plotTraj <- function() {  
  out <- calcTraj(); x1 <- out[,2]; y1 <- out[,3]; x2 <- out[,4]; y2 <- out[,5];
  out <- calcTraj(); getWinVal(scope="L"); # we need nt
  colours <- colorRampPalette(c("red", "orange", "yellow", 
            "green", "blue"), space="Lab")(nt+1);
  xrng <- range(c(x1,x2)); yrng <- range(c(y1,y2));
  plot(x1,y1,xlim=xrng,ylim=yrng,col=colours, pch=15);
  points(x2,y2,col=colours, pch=16); invisible(out); };

# Load PBS Modelling and initialize the GUI

require(PBSmodelling); require(ddesolve); createWin("2body.txt")
