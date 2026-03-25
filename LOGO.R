## config
current_path <- getwd()
DSSAT_path <- "/home/sinu/glue/DSSAT48"
AgMIP_path <- paste0(current_path,"/AgMIP-Calibration-Phase-IV")
if(!dir.exists(AgMIP_path)){
  system2("git",c("clone","https://github.com/sbuis/AgMIP-Calibration-Phase-IV.git",AgMIP_path))
}

input_path <- paste0(current_path,"/inputs")
code_path <- paste0(current_path,"/codes")
out_path <- paste0(current_path,"/output")
xfilename <- "X6019901.RIX"
CultivarID <- "IB0118"

nsite <- 6
ninit <- 5
rcodes <- list.files(code_path,full.name=TRUE)
rcodes <- rcodes[grep("main_rice",rcodes,invert=TRUE)]
invisible(lapply(rcodes,source))

## packages, functions
libraries <- c("DSSAT","dplyr","purrr","readxl","writexl","here")
for(l in libraries){
  if(!requireNamespace(l, quietly=TRUE)){
    install.packages(l, repos="https://cloud.r-project.org")
  }
  library(l, character.only=TRUE)
}
fl <- list.files(file.path(AgMIP_path,"R"),full.names=TRUE)
for(fn in fl){
  source(fn)
}
install_load()

## generate input data
print("generate input data")
change_site(input_path,ninit)
cases <- read.table('number_case_sites.txt')
ncombination <- nrow(cases)

## run calibration
if(!dir.exists(out_path))dir.create(out_path)
for (i in 1:ninit){
  if(!dir.exists(paste0(out_path,"/results",i)))dir.create(paste0(out_path,"/results",i))
  for (j in 1:ncombination){
    #calibration directory setting
    each_dir=paste0(current_path,"/case",i,"_",j)
    each_out_dir=paste0(out_path,"/results",i,"/case",i,"_",j)
    if(!dir.exists(each_dir))dir.create(paste0(each_dir,"/data"),recursive=TRUE)
    file.copy(paste0(input_path,"/",xfilename),each_dir,overwrite=TRUE)
    file.copy(paste0(input_path,"/cal_4_obs_LOGO_units.csv"),paste0(each_dir,"/data"),overwrite=TRUE)
    file.copy(paste0(input_path,"/data/cal_4_obs_LOGO_A_case_",j,".txt"),paste0(each_dir,"/data/cal_4_obs_LOGO_A.txt"),overwrite=TRUE)
    file.copy(paste0(input_path,"/xlsx",i,"/DSSAT_case_",j,".xlsx"),paste0(each_dir,"/data/DSSAT.xlsx"),overwrite=TRUE)

    #run calibration
    setwd(each_dir)

    GenotypeFileName="RICER048";CropName="RI";OD=each_dir;GD=paste0(DSSAT_path,"/Genotype");DSSATD=DSSAT_path
    #additionally need xfilename, CultivarID, AgMIP_path, code_path, library(here)
    source(paste0(code_path,"/main_rice.R"))

    file.rename(paste0(each_dir,"/results"), each_out_dir)
    unlink(each_dir,recursive=TRUE,force=TRUE)
  }
}

## generate cultivar file from calibrated data
setwd(out_path)
print("make CUL using calibrated data")
for (i in 1:ninit){
  compile_calibrated_results(i,1:ncombination)
  makeCUL(i)
}
setwd(current_path)

## run DSSAT using calibrated params
temp_path <- paste0(out_path,"/temp")
if(!dir.exists(temp_path))dir.create(temp_path)
if(!dir.exists(paste0(out_path,"/Evaluates")))dir.create(paste0(out_path,"/Evaluates"))
afilename <- gsub("RIX","RIA",xfilename)
file.copy(paste0(input_path,"/",afilename),paste0(temp_path,"/",afilename),overwrite=TRUE)
setwd(temp_path)
for (i in 1:ninit){
  file.copy(paste0(out_path,"/RICER048.CUL_",i), paste0(temp_path,"/RICER048.CUL"), overwrite=TRUE)
  for (j in 1:ncombination){
    if(is.na(cases[j,4]))next
    CULNO <- sprintf("CR1%03d",j)
    xfiletemp <- readLines(paste0(input_path,"/",xfilename)) %>%
                 gsub(CultivarID,CULNO,.) %>%
                 writeLines(paste0(temp_path,"/",xfilename))
    pos <- which(cases[,1]==j)
    trts <- lapply(strsplit(cases[pos,4],','),as.numeric)
    makeBatch(temp_path,xfilename,trts)
    system2(paste0(DSSAT_path,"/DSCSM048.EXE"), "B DSSBatch.v48")
    file.copy("Evaluate.OUT", paste0(out_path,"/Evaluates/Evaluate.OUT_init",i,"_case",j),overwrite=TRUE)
  }
}
setwd(current_path)

## calc likelihood & generate posterior mean parameter
setwd(out_path)
posterior(nsite,ninit)
setwd(current_path)
