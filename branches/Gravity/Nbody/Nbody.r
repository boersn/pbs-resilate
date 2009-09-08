# Original code from Sanae Rosen
# Modified by Jon Schnute

#*********************************************************************************************
#R code for displaying the N-body problem
#*********************************************************************************************
#TODO:
#what happens if they hit the same point?
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


#FIX
viewDataTable <- function(){
  data.entry(rawData)
  };
  
  
genData <- function(){
  getWinVal(scope="L");
  
  #Generate masses
  if(massDist==1) { #use lognormal dist
    mass <- rlnorm(n, meanlog=massMean, sdlog = massSD);
    }
  else { #all masses = 1
    mass <- rep(1, n); 
    }
    
  #generate positions (from -10 to 10)
  x <- runif(n, -10, 10);
  y <- runif(n, -10, 10);
  
  #generate velocities
  v <- rlnorm(n, meanlog=velMean, sdlog=velSD);
  u <- rlnorm(n, meanlog=velMean, sdlog=velSD);
  
  rawData <<- data.frame(x=x, y=y, u=u, v=v, mass=mass) #GLOBAL VARIABLE
  };
  

calcTraj <- function() {
  getWinVal(scope="L");

#  #center the system at (0, 0)
  if(CM) {
    newCoords <- centerSys(rawData$x, rawData$y, rawData$m);
    rawData$x <- newCoords[1:n];
    rawData$y <- newCoords[(1+n):(2*n)];
    };

  #center the velocity so the system has a net momentum of 0
  if(noMomentum) {
    newCoords <- centerSys(rawData$u, rawData$v, rawData$m);
    rawData$u <- newCoords[1:n];
    rawData$v <- newCoords[(1+n):(2*n)];
    };

  y0 <- c(rawData$x, rawData$y, rawData$u, rawData$v) #packing the data
  pars <- c(n=n, G=G, rawData$mass);
  times <- seq(0, t, by=t/nt);
  out <- dde(y=y0, func=calcGrad, times=times, parms=pars);
  out <- as.matrix(out); return(out); };

#takes in the coordinates and masses of the points, outputs new coordinates of the points so that the center of mass remains at (0, 0)
#also applicable for centering the velocity at 0
centerSys <- function(a, b, m) { 
  aNet <- sum(a*m)/sum(m);
  bNet <- sum(b*m)/sum(m);
  a <- a - aNet;
  b <- b - bNet;
  return(c(a, b))
  }; 


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
