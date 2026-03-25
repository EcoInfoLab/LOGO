##This is the function to run the DSSAT model.
DSSAT_wrapper <- function(param_values, situation, model_options, ...)
{
  situation <- as.character(situation)
  for(i in seq_along(model_options))
    assign(names(model_options)[i],model_options[[i]])
  setwd(OD);
  
  if(CropName=="WH"){
    GSTD <- 2
  }else{
    GSTD <- 3
  }

  print(param_values)
  if(is.vector(param_values)){
    param_values <- t(param_values)
  }else{
    param_values <- as.matrix(param_values)
  }
################### 1. Model Run ##################
  ModelRunNumber <- 1
  GenotypeChange(GD, DSSATD, OD, CropName, GenotypeFileName, CultivarID, ModelRunNumber, param_values); #Change the genotype file.
  if(exists("EcotypeID")) EcotypeChange(GD, DSSATD, OD, CropName, GenotypeFileName, EcotypeID, ModelRunNumber, param_values); #Change the genotype file.

  sit_split <- strsplit(situation,"_")
  if(exists("xfilename")){
    xfilename <- rep(xfilename,length(situation))
    trts <- situation
  }else if(length(sit_split[[1]])==1){
    xfilename <- paste0("TEST0001.",CropName,"X")
    trts <- situation
  }else{
    xfilename <- sapply(sit_split,"[",1)
    trts <- sapply(sit_split,"[",2)
  }
  makeBatch(OD,xfilename,trts)

  #model run
#  eval(parse(text = paste("system('",DSSATD,"/DSCSM048.EXE B ","DSSBatch.v48 > /dev/null 2>&1')",sep = '')));
  eval(parse(text = paste("system('",DSSATD,"/DSCSM048.EXE B ","DSSBatch.v48')",sep = '')));

  #result
  results <- list(sim_list = setNames(vector("list", length(situation)),nm = situation),
                  error = FALSE)
  attr(results$sim_list, "class") <- "cropr_simulation"

  #print(situation)
  for (sit in situation) {
    d <- data.frame(Date=NA,N_in_biomassHarvest=NA,ProteinContentGrain=NA,
             Date_BBCH10=NA,Date_BBCH30=NA,
             Date_BBCH55=NA,Date_BBCH90=NA,
             Biomass=NA,Grain_Number=NA,Grain_Yield=NA)
    results$sim_list[[sit]] <- d
  }
  if (file.exists("Evaluate.OUT")== F)
  {
    #error check 1
    warning("Evaluate.OUT not exists")
    results$error <- TRUE
  }else
  {
    #Read the output in evaluate file.
    eval(parse(text = paste("EvaluateOut<-readLines('",OD,"/Evaluate.OUT',n=-1)",sep = '')));
    #error check 2
    Error1Address<-match('NaN',EvaluateOut);
    Error2Address<-match("********",EvaluateOut);
    if (!is.na(Error1Address) || !is.na(Error2Address))
    {
      warning("NaN or ******** in Evaluate.OUT")
      results$error <- TRUE
    }
  
    Summarized <- summary_daily_values(OD,GSTD=GSTD)
    for (sit in situation) {
      # overwrite model input parameters of names contained in param_names with
      # values retrieved in param_values
      # run the model for the given situation
      # read the results and store the data.frame in result$sim_list[[situation]]
      i <- which(sit==situation)
      Date <- as.POSIXct(paste0(Summarized$Biomass[[i]][,1],Summarized$Biomass[[i]][,2]),"%Y%j",tz="UTC")
      Biomass_i <- Summarized$Biomass[[i]][,3]
      Date_last <- Date[length(Date)]
      Last_date_of_season <- as.POSIXct(paste0(year(Date_last),"1231"),format="%Y%m%d",tz="UTC")

      if(Date[length(Date)]!=Last_date_of_season){
        tmp <- Biomass_i[length(Biomass_i)]
        Biomass_i <- c(Biomass_i,tmp)
        Date <- c(Date,Last_date_of_season)
      }
      Date_BBCH10 <- rep(Summarized$Date_BBCH10[i],length(Date))
      Date_BBCH30 <- rep(Summarized$Date_BBCH30[i],length(Date))
      Date_BBCH55 <- rep(Summarized$Date_BBCH55[i],length(Date))
      Date_BBCH90 <- rep(Summarized$Date_BBCH90[i],length(Date))
      Grain_Yield_i <- rep(Summarized$Grain_Yield[i],length(Date))
      Grain_Number_i <- rep(Summarized$Grain_Number[i],length(Date))
      CNgrain_i <- rep(Summarized$CNgrain[i],length(Date))
      QNplante_i <- rep(Summarized$QNplante[i],length(Date))
      N_in_biomassHarvest_i <- ifelse(Biomass_i==0,0,QNplante_i / Biomass_i * 100)
      ProteinContentGrain_i <- CNgrain_i * 6.25
      #Biomass_i <- log(Biomass_i/10)
      Biomass_i <- Biomass_i/10

      d <- data.frame(Date=Date,N_in_biomassHarvest=N_in_biomassHarvest_i,ProteinContentGrain=ProteinContentGrain_i,
                 Date_BBCH10=Date_BBCH10,Date_BBCH30=Date_BBCH30,
                 Date_BBCH55=Date_BBCH55,Date_BBCH90=Date_BBCH90,
                 Biomass=Biomass_i,Grain_Number=Grain_Number_i,Grain_Yield=Grain_Yield_i/10)
      results$sim_list[[sit]] <- d
    }
  }
  return(results)
}
