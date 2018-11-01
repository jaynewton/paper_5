################################# 
#### Firm-Level Cross-Sectional Regression  
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_lottery_m.RData")

da_m_lm <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_lottery_m,by=c("ym","SecCode"))
da_m_lm <- da_m_lm[order(ym,SecCode),]

#### Decomposition Methodology (with One Candidate Explanatory Variable)
FUN_DECOMPOSITION <- function(model) {
  da_delta_m <- data.table(ym=ymd(names(summary(model)$coefficients[,,2][,1])),
                           delta_candidate=as.numeric(summary(model)$coefficients[,,2][,1])) 
  da_delta_m <- merge(da_delta_m,da_vari_m,by="ym")
  da_gamma_m <- merge(da_gamma_core_m,
                      da_delta_m[,.(gamma_candidate=cov(ret_e,delta_candidate*candidate)/var(be)),by=ym],
                      by="ym")
  da_gamma_m[,gamma_residual:=gamma_core-gamma_candidate]
  
  lm_sta <- matrix(NA,nrow=2,ncol=2)
  for (i in 1:2) {
    model_nw <- lm(da_gamma_m[[i+2]]~1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(lm_sta[,1]/da_gamma_m[,mean(gamma_core)]*100)
}

####  
lm_0 <- lmList(ret_e ~ be | ym , data=da_m_lm)
da_gamma_core_m <- data.table(ym=ymd(names(summary(lm_0)$coefficients[,,2][,1])),
                              gamma_core=as.numeric(summary(lm_0)$coefficients[,,2][,1])) 

####  
lm_1 <- lmList(be ~ lottery | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=lottery)]
FUN_DECOMPOSITION(lm_1)
