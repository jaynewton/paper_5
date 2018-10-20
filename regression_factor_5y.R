#################################
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_bab_m_5y.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_m.RData")
load("F:/我的论文/第五篇/RData/da_umd_m.RData")
load("F:/我的论文/第五篇/RData/da_iml_m.RData")
load("F:/我的论文/第五篇/RData/da_fmax_m.RData")

da_m <- merge(da_bab_m_5y,FF3F_A_m,by="ym")
da_m <- merge(da_m,da_umd_m,by="ym")
da_m <- merge(da_m,da_iml_m,by="ym")
da_m <- merge(da_m,da_fmax_m,by="ym")

FUN_NW <- function(regression_model) {
  return(coeftest(regression_model,vcov=NeweyWest(regression_model))[,c(1,3)])
} # NW denotes Newey-West t statistic 

####
da_m[,FUN_NW(lm(bab~1))]
da_m[,FUN_NW(lm(bab~mkt_e+smb+hml))]
da_m[,FUN_NW(lm(bab~mkt_e+smb+hml+umd))]
da_m[,FUN_NW(lm(bab~mkt_e+smb+hml+umd+iml))]

da_m[,FUN_NW(lm(bab~fmax))]
da_m[,FUN_NW(lm(bab~mkt_e+smb+hml+umd+iml+fmax))]

####
da_m[,FUN_NW(lm(fmax~1))]
da_m[,FUN_NW(lm(fmax~mkt_e+smb+hml))]
da_m[,FUN_NW(lm(fmax~mkt_e+smb+hml+umd))]
da_m[,FUN_NW(lm(fmax~mkt_e+smb+hml+umd+iml))]

da_m[,FUN_NW(lm(fmax~bab))]
da_m[,FUN_NW(lm(fmax~mkt_e+smb+hml+umd+iml+bab))]

