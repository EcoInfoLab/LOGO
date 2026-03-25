library(readxl)
library(writexl)

change_site<-function(input_path, ninit){

#obsfile
obs_ori <- read.table(paste0(input_path,'/cal_4_obs_LOGO_A_ori.txt'),header=T)
site_unique <- unique(obs_ori$Site)
nsite <- length(site_unique)

case <- 1
for(ns in 1:nsite){
  combns <- combn(1:nsite,ns)
  for(cb in seq_len(ncol(combns))){
    sites <- combns[,cb]
    site_names <- site_unique[sites]

    #new obs
    pos <- as.vector(obs_ori$Site%in% site_names)
    obs_new <- obs_ori[pos,]

    #write file
    obs_new_dir <- paste0(input_path,"/data")
    if(!dir.exists(obs_new_dir))dir.create(obs_new_dir)
    obs_new_fn <- paste0(obs_new_dir,"/cal_4_obs_LOGO_A_case_",case,".txt")
    write.table(obs_new,obs_new_fn,row.names=F,quote=F)

    #for summary
    sites_out <- paste0(sites,collapse=",")
    case = case + 1
  }
}

#xlsx file
for(initn in 1:ninit){
  xl_fn <- paste0(input_path,"/DSSAT_init_",initn,".xlsx")
  sheet_names <- excel_sheets(xl_fn)
  ori <- list()
  for(i in 1:6) ori[[i]]<-read_excel(xl_fn,i)
  names(ori) <- sheet_names

  case <- 1
  summ <- vector()
  for(ns in 1:nsite){
    combns <- combn(1:nsite,ns)
    for(cb in seq_len(ncol(combns))){
      sites <- combns[,cb]
      site_names <- site_unique[sites]

      #new xlsx
      xl_new <- ori

      #situation
      trts <- unique(obs_ori$Number[obs_ori$Site %in% site_names])
      trts_ll <- unique(obs_ori$Number[!obs_ori$Site %in% site_names])
      if(length(trts_ll)==0)trts_ll <- NA
      Number <- which(xl_new$"situation names"$"Situation Name" %in% trts)
      xl_new$"situation names" <- xl_new$"situation names"[Number,]

      #write file
      xl_new_dir <- paste0(input_path,"/xlsx",initn)
      if(!dir.exists(xl_new_dir))dir.create(xl_new_dir)
      xl_new_fn <- paste0(xl_new_dir,"/DSSAT_case_",case,".xlsx")
      write_xlsx(xl_new,xl_new_fn)

      #for summary
      trts_ll_out <- paste0(trts_ll,collapse=",")
      sites_out <- paste0(sites,collapse=",")
      summ <- c(summ,paste0(c(case,paste0(ns,"sites_case",cb),sites_out,trts_ll_out),collapse=" "))
      case = case + 1
    }
  }
}

write(summ,"number_case_sites.txt")
}
