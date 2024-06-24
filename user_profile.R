#-------------------------------------------------------------------
# Project: Do Non-Cognitive Skills Affect Employment Outcomes of Youth During the School-to-Work Transition
# Organization: SFedU Future Skills Research Lab
# Author:  Garen Avanesian, Valery Egorova
# Date: 24 June 2024
#-------------------------------------------------------------------


USERNAME    <- Sys.getenv("USERNAME") 
USER        <- Sys.getenv("USER")


if (USERNAME == "Valery") {   
  projectFolder  <- getwd()   
}

if (USER == "karavan88") {   
  projectFolder  <- getwd()   
}


stopifnot(dir.exists(projectFolder))

# set up key folders
documentation <-  file.path(projectFolder, "00_documentation")
inputData     <-  file.path(projectFolder, "01_input_data")
rcodes        <-  file.path(projectFolder, "02_codes")
outData       <-  file.path(projectFolder, "03_outputs/0301_data")
outTables     <-  file.path(projectFolder, "03_outputs/0302_tables")  
outFigures    <-  file.path(projectFolder, "03_outputs/0303_figures")


stopifnot(dir.exists(documentation))
stopifnot(dir.exists(inputData))
stopifnot(dir.exists(rcodes))
stopifnot(dir.exists(outData))
stopifnot(dir.exists(outTables))
stopifnot(dir.exists(outFigures))

