library(tibble)
library(DSSAT)

makeCUL <- function(num){
load(paste0('calibrated.Rdata',num))

trts <- 1:nrow(step7_calibrated_all)
VARNO <- paste0("CR1",formatC(trts,width=3,flag=0))
VRNAME <- paste0("case ",trts)
EXPNO <- "."
ECONO <- "IB0001"
cul_new_step7 <- tibble("VAR#"=VARNO,"VAR-NAME"=VRNAME,EXPNO=EXPNO,"ECO#"=ECONO,step7_calibrated_all)
v_fmt <- DSSAT:::cul_v_fmt("RICER")
cul_new_step7 <- cul_new_step7[,names(v_fmt)]
attr(cul_new_step7,"v_fmt")<-v_fmt

CUL_new_name <- paste0("RICER048.CUL_",num)
write_cul(cul_new_step7,CUL_new_name)
}
