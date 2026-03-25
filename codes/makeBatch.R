makeBatch <- function(OD,xfilenames,trts)
{
  batch <- paste0(OD,"/DSSBatch.v48")
  header <- paste0("$BATCH\n\n",
                 "@FILEX                                     ",
                 "                                           ",
                 "        TRTNO     RP     SQ     OP     CO")
  write(header, batch)

  for(i in seq_along(xfilenames)){
    template<-format(xfilenames[i],width=95)
    template<-paste0(template,"XX      0      0      0      0")
    trts_i <- unlist(trts[i])
    for(trt in trts_i){
      line <- gsub("XX",format(as.numeric(trt),width=2),template)
      write(line, batch, append=T)
    }
  }
}
