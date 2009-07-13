\name{convSlashes}
\alias{convSlashes}
\title{ Convert Slashes from UNIX to DOS }
\description{
  Convert slashes in a string from \code{"/"} to \code{"\\"} if the 
  operating system is \code{"windows"}. Do the reverse if the OS is
  \code{"unix"}.
}
\usage{
convSlashes(expr, os=.Platform$OS.type, addQuotes=FALSE)
}
\arguments{
  \item{expr}{ String value (usually a system pathway). }
  \item{os}{ operating system (either \code{"windows"} or \code{"unix"}). }
  \item{addQuotes}{ logical: if \code{TRUE}, enclose the string expression
    in escaped double quotation marks. }
}
\value{
  Returns the input string modified to have the appropriate slashes for the 
  specified operating system.
}
\author{ Rowan Haigh, Pacific Biological Station, Nanaimo BC }
\keyword{ character }
