library(purrr)
library(dplyr)
library(DSSAT)
posterior <- function(nsite,ninit){
#calc likelihood
calculate_log_likelihood <- function(sim,obs,cv=0.3){
    variance = (cv * obs) ** 2
    log_likelihood = -0.5 * log(2 * pi * variance) - ((obs - sim)**2) / (2 * variance)
    return(log_likelihood)
}

fl <- list.files("Evaluates")
fl_split <- strsplit(fl,"_")
inits <- fl_split %>% sapply(.,"[[",2) %>% gsub("init","",.) %>% as.numeric()
cases <- fl_split %>% sapply(.,"[[",3) %>% gsub("case","",.) %>% as.numeric()
likelihoods <- sapply(fl,function(fn){
  fn <- paste0("Evaluates/",fn)
  if(!file.exists(fn)) return(NA)
  ev <- read.table(fn,header=T,skip=1,comment.char="")
  lls <- calculate_log_likelihood(ev$HWAMS, ev$HWAMM)
  ll_each <- sum(lls)
  l_each <- exp(ll_each)
  return(l_each)
})
likelihoods <- data.frame(inits,cases,likelihoods)

#generate posterior mean
case <- read.table("../number_case_sites.txt")
culs_all <- map_dfr(1:ninit, ~ read_cul(paste0("RICER048.CUL_",.x)) %>% mutate(inits=.x)) %>% 
        bind_rows() %>% mutate(cases=as.numeric(gsub("^case\\s*","",`VAR-NAME`)))
culs_LOGO <- culs_all %>% select(-inits,-cases) %>% slice(0)

for(ns in 2:(nsite-1)){
  pos <- which(sapply(strsplit(case[,2],"sites_case"),"[[",1) == ns)
  case_num <- case[pos,1]
  wavg <- culs_all %>% filter(cases%in%case_num) %>% left_join(likelihoods,by=c('inits','cases')) %>% 
          mutate(prob=likelihoods/sum(likelihoods,na.rm=T)) %>% filter(!is.na(prob)) %>%
          summarise(across(P1:TCLDF, ~ weighted.mean(.x,w=prob,na.rm=TRUE))) %>%
          mutate(`VAR#`=sprintf("LG%04d",ns),
                `VAR-NAME`=sprintf("LOGO %d sites", ns),
                 EXPNO=".", "ECO#"="IB0001") %>% select(names(culs_LOGO))
  culs_LOGO <- bind_rows(culs_LOGO,wavg)
}
write_cul(culs_LOGO,"RICER048.CUL_LOGO")
}
