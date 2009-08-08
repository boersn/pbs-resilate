#*********************************************************************************************
#R code for setting and saving your PBS settings.
#features to implement:  Set extensions and associated filenames (done)
#save them to a file (done)
#automatically load them even if not changed
#set a standard working directory?
#navigate to standard working directory?
#checkbox to activate this feature?
#*********************************************************************************************

setChanges <- function() {
  getWinVal(scope="L");

  #set appropriate changes
  setPBSext("pdf", paste(pdf, "%f"))
  setPBSext("txt", paste(txt, "%f"))
  setPBSext("r", paste(r, "%f"))

  if(saveChanges==T) {
    writePBSoptions()
    }
  closeWin()
  
  }

loadPrevFile <- function() {
  #save the old extensions so you can revert with "cancel"
  oldExt <- list(getPBSext("pdf"), getPBSext("r"), getPBSext("txt")) 
  success <- is.null(readPBSoptions()) # A hack.  True if loaded successfully.  

  if (success){
    loadExt <- list(getPBSext("pdf"), getPBSext("r"), getPBSext("txt"))
    strLen <- nchar(oldExt) -3
    loadExt <- substr(oldExt, 0, strLen)
    setWinVal(list(pdf=loadExt[1], r=loadExt[2], txt=loadExt[3]))
    }
  }

# Load PBS Modelling and initialize the GUI

require(PBSmodelling); createWin("PBSoptGUI.txt");
