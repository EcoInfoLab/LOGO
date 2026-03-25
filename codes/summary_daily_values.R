summary_daily_values <- function(OD="/home/sinu/glue/DSSAT48/Wheat/",GSTD)
{
if(missing(GSTD))GSTD<-3
eval(parse(text = paste("EvaluateOut<-readLines('",OD,"/Evaluate.OUT',n=-1)",sep = '')));
pidat <- PIDAT(OD,GSTD)
EvaluateOut[3:length(EvaluateOut)] <- paste0(EvaluateOut[3:length(EvaluateOut)],pidat)
Evaluate <- read.table(textConnection(EvaluateOut),header=T,skip=1,comment.char="")
Evaluate[Evaluate == -99]<-NA
Summaryh <- read.table(paste0(OD,'/Summary.OUT'),skip=3,nrow=1,comment.char="")
Summary <- read.table(paste0(OD,'/Summary.OUT'),skip=4,comment.char="")
colnames(Summary) <- Summaryh[-1]

Date_Planting <- as.Date(as.character(Summary$PDAT),"%Y%j")
DOY_Planting <- as.numeric(format(Date_Planting,"%j"))
Date_BBCH10 <- as.Date(as.character(Summary$EDAT),"%Y%j")
  edats <- as.numeric(format(Date_BBCH10,"%j"))
  eyear <- as.numeric(format(Date_BBCH10,"%Y"))
  pidats <- Evaluate$PIDATS
  pidatm <- Evaluate$PIDATM
  piyear <- ifelse(edats>pidats,eyear+1,eyear)
PIDAPS <- as.numeric(as.Date(paste0(piyear,pidats),"%Y%j") - Date_Planting)
PIDAPM <- as.numeric(as.Date(paste0(piyear,pidatm),"%Y%j") - Date_Planting)
Evaluate <- cbind(Evaluate,PIDAPS,PIDAPM)

#Date_BBCH10 <- as.Date(as.character(Summary$EDAT),"%Y%j")
#Date_BBCH30 <- as.Date(paste0(piyear,pidats),"%Y%j")
#Date_BBCH55 <- as.Date(as.character(Summary$ADAT),"%Y%j")
#Date_BBCH90 <- as.Date(as.character(Summary$MDAT),"%Y%j")

Date_BBCH30 <- Evaluate$PIDAPS+DOY_Planting
Date_BBCH55 <- Evaluate$ADAPS+DOY_Planting
Date_BBCH90 <- Evaluate$MDAPS+DOY_Planting
#Date_BBCH90 <- rep(275,length(DOY_Planting)) #mdate fix
EDAP <- Date_BBCH10 - Date_Planting
Date_BBCH10 <- as.numeric(EDAP+DOY_Planting)

Date_BBCH55 <- ifelse(is.na(Date_BBCH55),-99,Date_BBCH55)
Date_BBCH90 <- ifelse(is.na(Date_BBCH90),-99,Date_BBCH90)

Biomass <- DailyOUT(OD,"PlantGro.OUT","CWAD",T)
#Grain_Yield <- DailyOUT(OD,"PlantGro.OUT","GWAD",T)
#Grain_Number <- DailyOUT(OD,"PlantGro.OUT","G.AD",T)
#CNgrain <- DailyOUT(OD,"PlantN.OUT","GN.D",T)
#QNplante <- DailyOUT(OD,"PlantN.OUT","CNAD",T) 
Grain_Yield <- Evaluate$HWAMS
Grain_Number <- Evaluate$H.AMS
QNplante <- Evaluate$CNAMS
CNgrain <- Evaluate$GN.MS

out <- list(Date_BBCH10=Date_BBCH10,Date_BBCH30=Date_BBCH30,
            Date_BBCH55=Date_BBCH55,Date_BBCH90=Date_BBCH90,
            Grain_Yield=Grain_Yield,Biomass=Biomass,Grain_Number=Grain_Number,
            CNgrain=CNgrain,QNplante=QNplante)
return(out)
}#function end
