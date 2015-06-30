# Taking cue from Roger Bivand's maptools:
.PBSresEnv <- new.env(FALSE, parent=globalenv())  # be sure to exportPattern("^\\.PBS") in NAMESPACE

.onAttach <- function(lib,pkg)
{
	pkg_info <- utils::sessionInfo( package="PBSresilate" )$otherPkgs$PBSresilate
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()
	
	userguide_path <- system.file( "doc/PBSresilate.pdf", package = "PBSresilate" )
	
	packageStartupMessage("
-----------------------------------------------------------
PBSresilate ", pkg_info$Version, " -- Copyright (C) 2008-2015 Fisheries and Oceans Canada

A basic user guide 'PBSresilate.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

All available PBS packages can be found at
http://code.google.com/p/pbs-software/

Type 'runResilate()' to start the model control GUI.
-----------------------------------------------------------

")
}
.onUnload <- function(libpath) {
	rm(.PBSresEnv)
}

# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
	".PBSmod",
	"addXY","addXZ","addYZ",
	"box",
	"colorRampPalette",
	"hist","histclr",
	"myGrad",
	"p23","pairs","par","PBSresi","points",
	"rect",
	"segments","size2d","size3d","solver","states",
	"t0","t1","type3d",
	"xyz",
	"y1","y2","y3"
	), package="PBSresilate")

