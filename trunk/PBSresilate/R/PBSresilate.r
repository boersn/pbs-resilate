#runResilate----------------------------2009-02-17
# Start the model choice for resilate (dynamic).
#-----------------------------------------------RH
runResilate =  function () {
	if (!require(PBSmodelling)) stop("!!!!!Install package PBSmodelling!!!!!")
	if (!require(PBSddesolve))  showAlert("Intall package 'PBSddesolve'")
	if (!require(deSolve))      showAlert("Intall package 'deSolve'")
	if (!require(rgl))          showAlert("Intall package 'rgl'")
	pkg="PBSresilate"
	wdir=.getPkgPath(pkg,"win")                 # window directory
	edir=.getPkgPath(pkg,"examples")            # examples directory
	tdir=tempdir(); tdir=gsub("\\\\","/",tdir)  # temporary directory
	temp=readLines(paste(wdir,"/resilateWin.txt",sep="")); 
	mods=.findPrefix(".r",edir)
	mrad=character(0)
	for (i in mods) 
		mrad=c(mrad,paste("\tradio name=model mode=character sticky=W function=resilate ",
			"text=\"",toupper(substring(i,1,1)),substring(i,2),"\" value=\"",
			i,"\" action=\"",i,"\"",sep=""))
	temp <- gsub("grid 0",paste("grid ",length(mrad)+1,sep=""),temp)
	temp=c(temp,mrad)
	twdf=paste(tdir,"/resilateWin.txt",sep="")
	writeLines(temp,con=twdf)
	createWin(twdf)
	invisible() }
#--------------------------------------runResilate

#resilate-------------------------------2009-02-17
# Resilate the universe.
#-----------------------------------------------RH
resilate =  function (model=NULL,hnam=NULL) {
	if (!require(PBSmodelling)) stop("!!!!!Install package PBSmodelling!!!!!")
	if (!require(PBSddesolve))  showAlert("Intall package 'PBSddesolve'")
	if (!require(deSolve))      showAlert("Intall package 'deSolve'")
	if (!require(rgl))          showAlert("Intall package 'rgl'")
	pkg="PBSresilate"
	PBSresi <<- list(pkg=pkg,func="resilate")
	if (is.null(model) && exists(".PBSmod",where=1) && !PBSmodelling:::.isReallyNull(.PBSmod,"window")) 
		model=getWinVal(winName="window")$model
	if (is.null(model) || model=="" || model=="none") model="lorenz"
	PBSresi$model<<-model

	wdir=.getPkgPath(pkg,"win")                 # window directory
	edir=.getPkgPath(pkg,"examples")            # examples directory
	tdir=tempdir(); tdir=gsub("\\\\","/",tdir)  # temporary directory
	wnam <- paste(model,"Win.txt",sep="")       # name of window description file
	doc  <- paste(model,"Doc.pdf",sep="")       # model documentation
	wdf  <- paste(edir,wnam,sep="/")            # window description file (model half)
	pwdf <- paste(wdir,"/plotWin.txt",sep="")   # plotting half of the wdf
	ehis <- paste(model,"Hist.txt",sep="")      # name of history file
	ehis <- paste(edir,ehis,sep="/")            # history file in examples directory
	wtmp <- paste(tdir,wnam,sep="/")
	rtmp <- paste(tdir,"resilate.r",sep="/")
	temp <- readLines(wdf); ptemp=readLines(pwdf)
	temp <- gsub("action=resilateWin.txt",paste("action=\"",wtmp,"\"",sep=""),temp)
	temp <- gsub("resilateDoc.pdf",paste(model,"Doc.pdf",sep=""),temp)
	temp <- gsub("action=resilate.r",paste("action=\"",rtmp,"\"",sep=""),temp)
	if (is.null(hnam) && file.exists(ehis)) hnam=ehis
	if (!is.null(hnam))
		ptemp <- gsub("#import=\"\"",paste("import=\"",hnam,"\"",sep=""),ptemp)
	writeLines(c(temp,ptemp),con=wtmp)
	createWin(wtmp)
	#.calcGrad()
	invisible() }
#-----------------------------------------resilate

.onClose=function() { 
	if (exists("PBSresi",envir=.GlobalEnv)) {
		save("PBSresi",file="PBSresi.rda",envir=.GlobalEnv) } }

#.calcGrad------------------------------2009-02-17
# Calculate the gradient
#-------------------------------------------ACB/RH
.calcGrad <- function() {
	getWinVal(winName="modwin",scope="L")  #extract variables from modwin GUI
	model=PBSresi$model
	edir=.getPkgPath("PBSresilate","examples")
	lenv=sys.frame(sys.nframe()) # local environment
	sys.source(paste(edir,"/",model,".r",sep=""),envir=lenv)
	# Functions to calculate gradient at a given time
	#eval(parse(text=paste("myGrad=",model,"Grad",sep="")))
	#initial values
	yinit <- c(y1=y1,y2=y2,y3=y3)
	if (!exists("states",envir=lenv)) states=c("y1","y2","y3")
	#solve ODE
	if (solver=="PBSddesolve") {
		x <- dde(y=yinit, func=myGrad, times=seq(t0,t1,timestep), hbsize=0)
	}
	else if (solver=="deSolve") {
		require(deSolve) || stop("deSolve is required")
		x <- lsoda(y=yinit, times=seq(from=t0, to=t1, by=timestep), func=myGrad, parms=NULL, rtol=1e-6, atol=1e-4)
		x <- as.data.frame(x)
	}
	# Calculate derivatives from x-values using myGrad
	myCalc=function(y) { val=myGrad(t=0,y=y,parms=NULL)[[1]]; names(val)=c("dy1","dy2","dy3"); return(val) }
	xdy = t(apply(x[,2:4],1,myCalc))
	x=cbind(x,xdy)
	PBSresi$x <<- x; PBSresi$states<<-states
	#.plotResi()
	invisible() }
#----------------------------------------.calcGrad

#.plotResi------------------------------2009-02-17
# Plot the resilation
#-----------------------------------------------RH
.plotResi=function() {
	xscale = function(x) {
		xmin=min(x,na.rm=TRUE); xmax=max(x,na.rm=TRUE)
		xsc=(x-xmin)/(xmax-xmin); attr(xsc,"lims")=c(xmin,xmax)
		xlab=pretty(x,n=5); xlab=xlab[xlab>=xmin & xlab<=xmax & !is.na(xlab)]
		xpos=(xlab-xmin)/(xmax-xmin)
		attr(xsc,"xpos")=xpos; attr(xsc,"xlab")=xlab
		return(xsc) }
	panel.hist <- function(x) {
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(usr[1:2], 0, 1.25))
		h <- hist(x,breaks=20,plot=FALSE)
		breaks <- h$breaks; nB <- length(breaks)
		y <- h$counts; y <- y/max(y)
		if (!is.null(histclr)) {clrs=histclr; bord="black"}
		else {clrs="aliceblue"; bord="midnightblue" }
		rect(breaks[-nB],0,breaks[-1],y,col=clrs,border=bord);  box() }
	panel.points <- function(x,y) {
		points(x,y,pch=15,col=clrs,cex=0.2) }
	panel.lines <- function(x,y) {
		N=length(x); x0=x[1:(N-1)]; y0=y[1:(N-1)]; x1=x[2:N]; y1=y[2:N]
		segments(x0,y0,x1,y1,col=clrs,lwd=1) }
	panel.balls <- function(x,y) {
		points(x,y,pch=21,col=0,bg=clrs,cex=0.5) }

	getWinVal(winName="modwin",scope="L")     #extract variables from modwin GUI
	if (!exists("PBSresi",where=1) || is.null(PBSresi$x)) {
		showAlert("Calculate the gradient first."); return() }
	else {
		unpackList(PBSresi,scope="L")
		lenv=sys.frame(sys.nframe()) # local environment
		edir=.getPkgPath("PBSresilate","examples")
		fld0=c("time","y1","y2","y3","dy1","dy2","dy3") # basic field from solver
		if (exists("states",envir=lenv))
			fld1=c("time",states,paste("d",states,sep=""))
		else fld1=fld0
		use=fld0[xyz]; see=fld1[xyz]
		#flds=names(x); useflds=intersect(flds,see)
		x=x[,use]; names(x)=see; n=nrow(x)
		clrs=colorRampPalette(c("red", "orange","yellow","green","blue"),space="Lab")(n)
		PBSresi$xnew<<-x; PBSresi$clrs <<- clrs
		if (p23==2) {
			resetGraph()
			if (type3d=="p") {upper=lower=panel.points }
			else if (type3d=="l") {upper=lower=panel.lines}
			else {upper=panel.balls; lower=panel.lines }
			pairs(x,gap=0,label.pos=.92,diag.panel=panel.hist,upper.panel=upper,lower.panel=lower)
			#pairs(x, pch=15, gap=0, cex=.2, col="blue") # pch=183 does not work on UNIX, pch="." is too small
		} else if (p23==3) {
			if (sum(xyz)!=3) {showAlert("Choose 3 states"); return()}
			xs=apply(x,2,xscale); xs=as.data.frame(xs,row.names=rownames(x))
			xsc=sapply(x,xscale,simplify=FALSE); PBSresi$xsc<<-xsc
			xs=as.data.frame(xsc); PBSresi$xs<<-xs
			#par3d(windowRect=c(70,90,700,720)) # cannot control the size of the initial 3D window
			plot3d(xs,col=clrs,size=size3d,top=TRUE,type=type3d,axes=FALSE,xlab="",ylab="",zlab="") # s=spheres, p=points, l=lines
			for (i in 1:3) {
				xpos=attributes(xsc[[i]])$xpos; xlab=attributes(xsc[[i]])$xlab
				axis3d(switch(i,'x--','y--','z-+'),at=xpos,labels=xlab)
					#adj=switch(i,c(.5,-.6),c(.5,-.6),c(-.2,.5))) 
				mtext3d(see[i],switch(i,'x--','y--','z-+'),line=2) }
			bb=par3d()$bbox
			if (addXY) plot3d(xs[,1],xs[,2],rep(bb[6],n),size=size2d,col=clrs,add=TRUE,axes=FALSE)
			if (addYZ) plot3d(rep(bb[2],n),xs[,2],xs[,3],size=size2d,col=clrs,add=TRUE,axes=FALSE)
			if (addXZ) plot3d(xs[,1],rep(bb[4],n),xs[,3],size=size2d,col=clrs,add=TRUE,axes=FALSE)
		}
	}
}
#----------------------------------------.plotResi

#2009-02-03-------------------------------------RH
# Executes the action created by a widget.
#Start doAction-----------------------------------
doAction=function(act,envir=.GlobalEnv){
	if (missing(act)) {
		if(is.null(.PBSmod$.activeWin)) return()
		act=getWinAct()[1] }
	if(is.null(act) || act=="") return()
	expr=gsub("`","\"",act)
	eval(parse(text=expr),envir=envir)
	invisible(act) }
#-------------------------------------End doAction

#viewCode-------------------------------2009-02-16
# View the package R code on the fly.
#-----------------------------------------------RH
viewCode=function(pkg="PBSresilate"){
	eval(parse(text=paste("if(!require(",pkg,",quietly=TRUE)) stop(\"",pkg," package is required\")",sep="")))
	tdir <- tempdir(); tdir <- gsub("\\\\","/",tdir)                      # temporary directory for R
	pkgO=ls(paste("package:",pkg,sep=""),all=TRUE)                        # package objects
	z=sapply(pkgO,function(x){f=get(x);is.function(f)}); pkgF=names(z)[z] # package functions
	bad=regexpr("[\\|()[{^$*+?<-]",pkgF); pkgF=pkgF[bad<0]                # get rid of weird names
	if (length(pkgF)==0) {showAlert(paste(pkg,"has no recognizable functions")); return()}
	dots=regexpr("^\\.",pkgF); pkgF0=pkgF[dots==1]; pkgF1=pkgF[dots!=1]
	code=c(paste("#",pkg,"Functions"),paste("#",paste(rep("-",nchar(pkg)+10),collapse="")))
	for (i in c(pkgF1,pkgF0)) {
		expr=paste("fun=deparse(",pkg,"::",i,"); fun[1]=paste(\"",i,"\",fun[1],sep=\" = \",collapse=\"\"); code=c(code,fun)",sep="")
		eval(parse(text=expr)) }
	fname=paste(tdir,"/",pkg,".r",sep="")
	writeLines(code, fname); openFile(convSlashes(fname))
	invisible(code) }
#----------------------------------------viewCode

#convSlashes---------------------------2009-02-16
# Convert unix "/" to R's "\\" if OS is windows.
#-----------------------------------------------RH
convSlashes=function(expr, os=.Platform$OS.type, addQuotes=FALSE){
	if (os=="windows") expr=gsub("/","\\\\",expr)
	else expr=gsub("\\\\","/",expr)
	if (addQuotes) expr=paste("\"",expr,"\"",sep="")
	return(expr) }

.findSquare=function(nc) {
	sqn=sqrt(nc); m=ceiling(sqn); n=ceiling(nc/m)
	return(c(m,n)) }

.getPkgPath=function(pkg="PBSresilate", dir="examples"){
	return(paste(system.file(package=pkg),dir,sep="/")) }

.findPrefix=function(suffix,path="."){ # to replace Anisa's findPrefix
	spat=gsub("\\.","\\\\\\.",suffix)
	sfiles=list.files(path,pattern=paste(spat,"$",sep=""),ignore.case=TRUE)
	pref=substring(sfiles,1,nchar(sfiles)-nchar(suffix))
	return(pref) }

#showPacks------------------------------2009-02-17
# Show packages that need to be installed.
#-----------------------------------------------RH
showPacks <- function(packs=c("PBSmodelling","PBSmapping","PBSddesolve","rgl","deSolve",
         "akima","deldir","sp","maptools","KernSmooth")) {
	warn <- options()$warn
	options(warn = -1)
	Apacks = .packages(all.available = TRUE)   # all packages
	Ipacks = sort(findPat(packs,Apacks))       # installed packages that match those required
	Mpacks = sort(setdiff(packs,Ipacks))       # missing packages
	if (length(Mpacks)==0)
		showAlert("Congratulations! All specified pakages installed.","Package Check","info")
	else {
		mess=paste("You need to install these packages:\n     ",paste(Mpacks,collapse="\n     "),sep="")
		showAlert(mess,"Package Check","warning")
	}
	options(warn = warn)
	invisible(list(Apacks=Apacks,Ipacks=Ipacks,Mpacks=Mpacks)) }

