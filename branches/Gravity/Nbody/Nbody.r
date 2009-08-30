# Original code from Sanae Rosen
# Modified by Jon Schnute

#*********************************************************************************************
#R code for displaying the 2-body problem
#*********************************************************************************************
#TODO:
#make trajectory time-dependent
#what happens if they hit the same point? they stop moving
#

calcGrad <- function(t, y0, parms) { # gradient from Newton's law
  # unpack the states and parms
  n <- parms[1];
  x <- y0[1:n]; y <- y0[(1+n):(2*n)]; u <- y0[(1+2*n):(3*n)]; v <- y0[(1+3*n):(4*n)];
  G <- parms[2]; m <- parms[3:(2+n)];
  
  du <- c()
  dv <- c()


  for(i in c(1:n)){
    #the law of gravity
    r2 <- (x[i] - x[-i])^2 + (y[i] - y[-i])^2; r3 <- r2^(3/2);
    r <- c(x[-i] - x[i],y[-i] - y[i]);
    r <- matrix(r, nrow=n-1, ncol=2); #matrix-ify to make it easier.  Row: the particle.  Column: the dimension
    
    acc_local <- G*m[-i]*colSums(r/r3)

    du <- c(du, acc_local[1]);
    dv <- c(dv, acc_local[2]);
    };

  dx <- u; dy <- v;
  
  return(c(dx, dy, du, dv));  };


#Accept the data table.  Find out the number of particles.
#Eventually, check that the data is correct/workable
initDataTable <- function(){
  rawData <<- data.frame(mass=1, x=c(0,0,1), y=c(0,1,0), u=c(0,0,1), v=c(-1,1,0)); #global variable
  data.entry(rawData)  
  n <<- length(rawData$mass)#how many entries
  };
  

calcTraj <- function() {
  getWinVal(scope="L");

#  #center the system at (0, 0)
#  if(CM) {
#    newCoords <- centerSys(wx1, wy1, m1, wx2, wy2, m2);
#    wx1 <- newCoords[1]; wy1 <- newCoords[2]; wx2 <- newCoords[3]; wy2 <- newCoords[4];
#    };

  #center the velocity so the system has a net momentum of 0
#  if(noMomentum) {
#    newVels <- centerSys(wu1, wv1, m1, wu2, wv2, m2)
#    wu1 <- newVels[1]; wv1 <- newVels[2]; wu2 <- newVels[3]; wv2 <- newVels[4];
#    };

  y0 <- c(rawData$x, rawData$y, rawData$u, rawData$v) #packing the data
  pars <- c(n=n, G=G, rawData$mass);
  times <- seq(0, t, by=t/nt);
  out <- dde(y=y0, func=calcGrad, times=times, parms=pars);
  out <- as.matrix(out); return(out); };

#takes in the coordinates and masses of the points, outputs new coordinates of the points so that the center of mass remains at (0, 0)
#also applicable for centering the velocity at 0
#centerSys <- function(a1, b1, m1, a2, b2, m2) { 
#  aNet <- (a1*m1 + a2*m2)/(m1+m2);
#  bNet <- (b1*m1 + b2*m2)/(m1+m2);
#  a1 <- a1 - aNet;
#  a2 <- a2 - aNet;
#  b1 <- b1 - bNet;
#  b2 <- b2 - bNet;
#  return(c(a1, b1, a2, b2))
#  };


plotTraj <- function() {  
  out <- calcTraj(); getWinVal(scope="L"); # we need nt
  colList <- c("red", "orange", "yellow", 
            "green", "blue", "black") #only supports 6 masses right now
  x <- out[,2:(n+1)];
  y <- out[,(2+n):(2*n+1)];
  xrng <- range(x); yrng <- range(y);
  plot(0,0,xlim=xrng, ylim=yrng, col=colList[1], pch=15);
  for(i in c(1:n)){
    points(x[,i], y[,i], col=colList[i], pch=15); 
    };
  invisible(out)
  };

# Load PBS Modelling and initialize the GUI

require(PBSmodelling); require(ddesolve); createWin("Nbody.txt")
