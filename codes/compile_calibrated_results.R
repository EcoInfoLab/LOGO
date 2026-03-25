library(readxl)
compile_calibrated_results <- function(num,jobnumbers){
base <- getwd()
jobnames <- paste0("results",num,"/case",num,"_",jobnumbers)

#get parameter range from xlsx
major <- read_excel(paste0(base,"/",jobnames[1],"/LOGO/A/DSSAT.xlsx"),2)
candi <- read_excel(paste0(base,"/",jobnames[1],"/LOGO/A/DSSAT.xlsx"),3)
paramnames <- c(major$'name of the parameter', candi$'name of the parameter')
lower <- c(major$'lower bound', candi$'lower bound')
upper <- c(major$'upper bound', candi$'upper bound')
ranges <- as.data.frame(rbind(lower,upper))
colnames(ranges) <- paramnames

#number_case_sites
#case_path <- "/home/sinu/glue/DSSAT48/Uncertainty_Rice_input/change_site/data/"
#number_case <- read.table('../number_case_sites.txt')
#number_case[,2] <- gsub(',','_',number_case[,2])

#calibrated params, if not exists, NA
load_calibrated_params <- function(path, paramnames){
  if (file.exists(path)) {
    load(path)
    df <- data.frame(c(res$final_values, res$forced_param_values))
    if (nrow(df) == 0) {
      df <- data.frame(matrix(NA, nrow = 1, ncol = length(paramnames)))
      colnames(df) <- paramnames
    }
  } else {
    df <- data.frame(matrix(NA, nrow = 1, ncol = length(paramnames)))
    colnames(df) <- paramnames
  }
  return(df[, paramnames])
}

#make data.frame
#step6_calibrated_all <- data.frame()
step7_calibrated_all <- data.frame()

#read Rdata and summary
for(i in seq_along(jobnumbers)){
  print(jobnames[i])
#  step6_i <- "step6_calibrated_all"
  step7_i <- "step7_calibrated_all"

#  step6_Rdata <- paste0(base,"/",jobnames[i],"/LOGO/A/step6/group_yield/optim_results.Rdata")
#  step6_calibrated <- load_calibrated_params(step6_Rdata,paramnames)
#  eval(parse(text=paste0(step6_i," <- rbind(", step6_i, ",step6_calibrated)")))

  step7_Rdata <- paste0(base,"/",jobnames[i],"/LOGO/A/step7/optim_results.Rdata")
  step7_calibrated <- load_calibrated_params(step7_Rdata,paramnames)
  eval(parse(text=paste0(step7_i," <- rbind(", step7_i, ",step7_calibrated)")))
}

#save calibrated results
vars_to_save <- "step7_calibrated_all"
Rdata_name <- paste0("calibrated.Rdata",num)
save(list=vars_to_save,file=Rdata_name)
}
