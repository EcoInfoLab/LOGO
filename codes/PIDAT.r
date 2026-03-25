PIDAT <- function(OD,GSTD)
{
    if(missing(GSTD))GSTD<-3
    #PIDATS
    PlantGroFile <- paste0(OD,'/PlantGro.OUT')
    if(file.exists(PlantGroFile)){
      PlantGro <- readLines(PlantGroFile)
      Header1 <- grep("@YEAR",PlantGro)
      Header2 <- grep("\\*DSSAT",PlantGro)
      Header2 <- c(Header2[-1],length(PlantGro))
      Headers <- cbind(Header1, Header2)
      return_pidat <- function(x,txt){
        st <- x[1]; ed <- x[2]-1
        y<-read.table(textConnection(txt[st:ed]),header=T,comment.char="")
        return(y$DOY[which(y$GSTD==GSTD)[1]])
      }
      pidats <- apply(Headers,1,return_pidat,PlantGro)
    }else{
      Evaluate <- readLines(paste0(OD,'/Evaluate.OUT'))
      firstLinePos <- grep("^\\*EVALUATION",Evaluate)
      Evaluate <- Evaluate[firstLinePos:length(Evaluate)]
      ntrt <- length(Evaluate) - 3
      pidats <- rep(-99,ntrt)
    }

    #PIDATM
    pidatm <- rep(-99,length(pidats))
    CultivarBatchFile <- paste0(OD,"/DSSBatch.v48")
    if(file.exists(CultivarBatchFile)){
      DSSBatch <- read.table(CultivarBatchFile,skip=1,header=T)
      AfileNames <- gsub("X$","A",DSSBatch[,1])
      for(AfileName in unique(AfileNames)){
        pos1 <- which(AfileName==AfileNames)
        trt <- DSSBatch[pos1,2]
        if(file.exists(AfileName)){
          Afile <- read.table(AfileName,header=T,comment.char="")
          Afiletrt <- Afile$X.TRNO
          AfilePIDAT <- Afile$PIDAT
          pos2 <- which(Afiletrt%in%trt)
          pos3 <- which(trt%in%Afiletrt)
          if(length(AfilePIDAT[pos2])>0 & length(pos3)>0)
            pidatm[pos1[pos3]]<-AfilePIDAT[pos2]
        }
      }
    }
    pidats <- format(pidats,width=8)
    pidatm <- format(pidatm,width=8)
    pidat <- c("  PIDATS  PIDATM", paste0(pidats,pidatm))
#      EvaluateOut[3:length(EvaluateOut)] <- paste0(EvaluateOut[3:length(EvaluateOut)],pidat)
    return(pidat)
}
