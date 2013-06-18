\name{rtget}
\alias{rtget}
\alias{rtcall}
\alias{rtprint}
\alias{rtput}
\alias{rlisp}
\title{
  Get/Print Objects From or Put Objects Into Temporary Work Environment
}
\description{
  These functions are wrappers to the PBSmodelling accessor functions that
  get/print objects from or put objects into a temporary work environment,
  in this case \code{.PBSresEnv}. Working objects include \code{PBSresi},
  which acts as a storage object for many of the functions.
}
\usage{
rtget(...)
rtcall(...)
rtprint(...)
rtput(...)
rlisp(...)
}
\arguments{
  \item{...}{For \code{rtget} through to \code{rtput}, the only free argument is: \cr
    \code{x} -- name (with or without quotes) of an object to retrieve or store 
    in the temporary environment; cannot be represented by a variable. \cr
    Fixed arguments: \code{penv = parent.frame(), tenv = .PBSresEnv} \cr
    See \code{\link[PBSmodelling]{tget}} for additional information.

    For \code{rlisp}, there is only one fixed argument: \cr
    \code{pos = .PBSresEnv} \cr
    All other arguments are available -- see \code{\link[PBSmodelling]{lisp}} }
}
\details{
  These accessor functions were developed as a response to the CRAN
  repository policy statement: \dQuote{Packages should not modify the
  global environment (user's workspace).}
  
}
\note{
  Additional wrapper functions to access functions in \code{.PBSresEnv}
  are named with the prefix \code{.win} (none at the moment).
}
\value{
  Objects are retrieved from or sent to the temporary working 
  environment to/from the place where the function(s) are called.
  Additionally, \code{rtcall} invisibly returns the object without 
  transferring, which is useful when the object is a function that the
  user may wish to call, for example, \code{rtcall(myfunc)()}.
}
\references{
  CRAN Repository Policy: 
  \url{http://cran.r-project.org/web/packages/policies.html}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
\code{\link[PBSmodelling]{tget}} and \code{\link[PBSmodelling]{lisp}} in \pkg{PBSmodelling}
}
\keyword{manip}
\keyword{environment}

