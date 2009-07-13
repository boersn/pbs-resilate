\name{resilate}
\alias{resilate}
\title{Resilate 3-State Models}
\description{
  Display resilations controlled by an interactive GUI.
}
\usage{
resilate(model=NULL, hnam=NULL) 
}
\arguments{
  \item{model}{string name of a 3-state model.}
  \item{hnam}{string name of a history file.}
}
\details{
  The function \code{resilate()} creates an interactive GUI that can be used
  to display resilations of a 3-state model over time.
\cr \cr
  \bold{The GUI controls:} \cr
  \tabular{ll}{
  \bold{Model}        \tab \cr
  \code{Lorenz}       \tab Use the Lorenz (1963) model for atmospheric currents.\cr
  \code{Hastings}     \tab Use the Hastings & Powell (1991) model for linear food chains.\cr
  \code{Edwards}      \tab Use the Edwards & Brindley (1999) model for plankton dynamics.\cr
  \code{Ludwig}       \tab Use the Ludwig, Jones & Holling (1978) model for spruce buzzworm outbreak systems.\cr
  \bold{Solver}       \tab \cr
  \code{deSolve}      \tab Use Petzold & Hindmarsh's \code{lsoda} function for ordinary differential equations.\cr
  \code{PBSddesolve}  \tab Use Couture-Beil & Wood's \code{dde} function for delay-differential equations.\cr
  \bold{Parameters}   \tab \cr
  \code{Model}        \tab Parameter models (control parameters are different for each model).\cr
  \code{Time}         \tab Time parameters.\cr
  \code{start}        \tab First time value.\cr
  \code{stop}         \tab Last time value.\cr
  \code{step}         \tab Time step at which to evaluate \code{y1, y2, y3}.\cr
  \bold{Initial State Values}  \tab \cr
  \code{y1,y2,y3}     \tab Initial values for \code{y1}, \code{y2}, and \code{y3}.\cr
  \bold{Plot 2D or 3D?} \tab \cr
  \code{2D}           \tab Two-dimensional (flat) \code{pairs} plot.\cr
  \code{3D}           \tab Three-dimensional plot using the \code{rgl} package function \code{plot3d}.\cr
  \code{X-Y plane}    \tab On the 3D plot, superimpose the plot coordinates on the \emph{x-y} plane (flatten \emph{z}).\cr
  \code{Y-Z plane}    \tab On the 3D plot, superimpose the plot coordinates on the \emph{y-z} plane (flatten \emph{x}).\cr
  \code{X-Z plane}    \tab On the 3D plot, superimpose the plot coordinates on the \emph{x-z} plane (flatten \emph{y}).\cr
  \code{size2d}       \tab Size of points in 2D-panels of 3D plot.\cr
  \code{size3d}       \tab Size of points/spheres in 3D plot.\cr
  \code{Display points} \tab Type of points to plot: s = spheres, p = points, l = lines.\cr
  \code{hist}         \tab Histogram bar colour.\cr
  \code{states}       \tab Choose states to plot (\code{time}, \code{y1}, \code{y2}, \code{y3}, \code{dy1}, \code{dy2}, \code{dy3}).\cr
                      \tab Note: choose only 3 states for a 3D plot.\cr
  \code{Calc}         \tab Button to recalculate the state values and derivatives given the input parameters and time values.\cr
  \code{Plot}         \tab Button to plot the chosen states in the specified dimension.\cr
  \bold{History}      \tab History widget.\cr
  }
}
\references{
Edwards, A.M. and Brindley, J. (1999) Zooplankton mortality and the dynamical behaviour of
  plankton population models. \emph{Bulletin of Mathematical Biology} \bold{61}, 303--339.

Hastings, A. and Powell, T. (1991) Chaos in a three-species food chain. \emph{Ecology} 
  \bold{72(3)}, 896--903.

Lorenz, E.N. (1963) Deterministic non-periodic flows. \emph{Journal of Atmospheric Science}
  \bold{20}, 130--141. \cr
  \url{http://planetmath.org/encyclopedia/LorenzEquation.html}

Ludwig, D., Jones, D.D. and Holling, C.S. (1978) Qualitative analysis of insect outbreak systems: 
  the spruce budworm and forest. \emph{The Journal of Animal Ecology} \bold{47(1)}, 315--332.
}
\keyword{hplot}
