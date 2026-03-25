DailyOUT <- function(OD,DailyOUTFileName='PlantGro.OUT',VAR,AllDaily=FALSE)
{
    DailyOUT <- readLines(paste0(OD,'/',DailyOUTFileName))
    Header1 <- grep("@YEAR",DailyOUT)
    Header2 <- grep("\\*DSSAT",DailyOUT)
    Header2 <- c(Header2[-1],length(DailyOUT)+1)
    DailyOUTTrt <- grep("TREATMENT",DailyOUT, value=T)
    DailyOUTTrt <- as.numeric(substr(DailyOUTTrt, 11, 13))

    DailyOUT.df <- list()
    for(i in 1:length(Header1)){
      temp <- DailyOUT[Header1[i]:(Header2[i]-1)]
      DailyOUT.df[[i]] <- read.table(textConnection(temp),header=T,comment.char="")
    }
    DailyOUTYEAR <- lapply(DailyOUT.df,"[[","X.YEAR")
    DailyOUTDOY <- lapply(DailyOUT.df,"[[","DOY")
    DailyOUTVAR <- lapply(DailyOUT.df,"[[",VAR)

    CultivarBatchFile=paste0(OD,"/DSSBatch.v48")
    if(AllDaily){
      result <- list()
      for(i in 1:length(Header1)){
        result[[i]] <- cbind(DailyOUTYEAR[[i]],DailyOUTDOY[[i]],DailyOUTVAR[[i]])
      }
    }else{
      DSSBatch <- read.table(CultivarBatchFile,skip=1,header=T)
      TfileNames <- gsub("X$","T",DSSBatch[,1])
      out <- vector()
      for(TfileName in unique(TfileNames)){
        pos1 <- which(TfileName==TfileNames)
        trt <- DSSBatch[pos1,2]
        if(file.exists(TfileName)){
          Tfile <- read.table(TfileName,header=T,comment.char="")
          Tfiletrt <- Tfile$X.TRNO
          for(i in pos1){
            trt <- DailyOUTTrt[i]
            TfileDATE <- formatC(Tfile[which(Tfile$X.TRNO==trt),"DATE"],width=5,flag=0)
            VARM <- Tfile[which(Tfile$X.TRNO==trt),VAR]
            DailyOUTDATE <- paste0(substr(DailyOUTYEAR[[i]],3,4),formatC(DailyOUTDOY[[i]],width=3,flag=0))
            VARS <- DailyOUTVAR[[i]][DailyOUTDATE%in%TfileDATE]
            if(length(VARS)<length(VARM)) VARS[(length(VARS)+1):length(VARM)]<-NA
            out <- rbind(out,cbind(VARS,VARM))
          }
        }
      }
      colnames(out) <- paste0(VAR,c("S","M"))
      result <- as.data.frame(out)
    }
    return(result)
}

DailyOUTPlot <- function(OD,DailyOUTFileName='PlantGro.OUT',VAR,trt)
{
    DailyOUT <- readLines(paste0(OD,'/',DailyOUTPUTFileName))
    Header1 <- grep("@YEAR",DailyOUT)
    Header2 <- grep("\\*DSSAT",DailyOUT)
    Header2 <- c(Header2[-1],length(DailyOUT))
    DailyOUTTrt <- grep("TREATMENT",DailyOUT, value=T)
    DailyOUTTrt <- as.numeric(substr(DailyOUTTrt, 11, 13))

    DailyOUT.df <- list()
    for(i in 1:length(Header1)){
      temp <- DailyOUT[Header1[i]:(Header2[i]-1)]
      DailyOUT.df[[i]] <- read.table(textConnection(temp),header=T,comment.char="")
    }
    DailyOUTYEAR <- lapply(DailyOUT.df,"[[","X.YEAR")
    DailyOUTDOY <- lapply(DailyOUT.df,"[[","DOY")
    DailyOUTVAR <- lapply(DailyOUT.df,"[[",VAR)

    CultivarBatchFile="DSSBatch.v48"
    DSSBatch <- read.table(CultivarBatchFile,skip=1,header=T)
    TfileNames <- gsub("X$","T",DSSBatch[,1])
    out <- vector()
    TfileName <- TfileNames[trt]
      if(file.exists(TfileName)){
        Tfile <- read.table(TfileName,header=T,comment.char="")
        Tfiletrt <- Tfile$X.TRNO
        for(i in trt){
          trt_each <- DailyOUTTrt[i]
          TfileDATE <- formatC(Tfile[which(Tfile$X.TRNO==trt_each),"DATE"],width=5,flag=0)
          VARM <- Tfile[which(Tfile$X.TRNO==trt_each),VAR]
          DailyOUTDATE <- paste0(substr(DailyOUTYEAR[[i]],3,4),formatC(DailyOUTDOY[[i]],width=3,flag=0))
          VARS <- DailyOUTVAR[[i]][DailyOUTDATE%in%TfileDATE]
          r<-range(VARM,DailyOUTVAR[[i]])
          plot(as.Date(DailyOUTDATE,"%y%j"),DailyOUTVAR[[i]],type='l',ylim=r,xlab='Date',ylab=VAR)
          points(as.Date(TfileDATE,"%y%j"),VARM)
        }
      }
}
