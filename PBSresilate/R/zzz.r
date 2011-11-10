.onLoad <- function(lib,pkg)
{
	pkg_info <- utils::sessionInfo( package="PBSresilate" )$otherPkgs$PBSresilate
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- "unkown"
	
	userguide_path <- system.file( "doc/PBSresilate.pdf", package = "PBSresilate" )
	
	cat("
-----------------------------------------------------------
PBSresilate", pkg_info$Version, "-- Copyright (C) 2008-2011 Fisheries and Oceans Canada

A basic user guide 'PBSresilate.pdf' is located at 
", userguide_path, "

Packaged on", pkg_date, "
Pacific Biological Station, Nanaimo

Type 'runResilate()' to start the model control GUI.
-----------------------------------------------------------


")
}

