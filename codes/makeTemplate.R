makeTemplate<-function(obsFileName, templateFileName)
{
  obs <- read.table(obsFileName,header=T)
  num <- unique(obs$Number)
  num_last <- sapply(num,function(x){w<-which(obs$Number==x);return(w[length(w)])})
  date_last <- as.Date(obs$Date[num_last],"%d/%m/%Y")
  date_last2 <- paste0("31/12/",format(date_last,"%Y"))
  useCat <- rep("train",nrow(obs))
  obs$Date[num_last]<-date_last2
  template <- cbind(obs[,1:5],useCat,obs[,6:ncol(obs)])
  if(!("Date_BBCH10" %in% colnames(template))){
    Date_BBCH10 <- rep(NA,nrow(obs))
    template <- cbind(template[,1:8],Date_BBCH10,template[,9:ncol(template)])
  }
  template[,8:ncol(template)]<-NA

  write.table(template,templateFileName,row.names=F,quote=F)
}

