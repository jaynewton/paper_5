################################# 
#### Firm-Level Cross-Sectional Regression  
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_ivol_m.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_cosk_5y.RData")

da_m_lm <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_ivol_m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_mom_m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_cosk_5y,by=c("ym","SecCode"))

da_m_lm[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),ivol=log(ivol))]
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
lm_1 <- lmList(be ~ max_ret | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=max_ret)]
FUN_DECOMPOSITION(lm_1)

lm_2 <- lmList(be ~ size | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=size)]
FUN_DECOMPOSITION(lm_2)

lm_3 <- lmList(be ~ BM | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=BM)]
FUN_DECOMPOSITION(lm_3)

lm_4 <- lmList(be ~ mom | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=mom)]
FUN_DECOMPOSITION(lm_4)

lm_5 <- lmList(be ~ ret_e_tm | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=ret_e_tm)]
FUN_DECOMPOSITION(lm_5)

lm_6 <- lmList(be ~ illiq | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=illiq)]
FUN_DECOMPOSITION(lm_6)

lm_7 <- lmList(be ~ ivol | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=ret_e_tm)]
FUN_DECOMPOSITION(lm_7)

lm_8 <- lmList(be ~ isk | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=illiq)]
FUN_DECOMPOSITION(lm_8)

lm_9 <- lmList(be ~ cosk_1 | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=cosk_1)]
FUN_DECOMPOSITION(lm_9)

lm_10 <- lmList(be ~ cosk_2 | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=cosk_2)]
FUN_DECOMPOSITION(lm_10)

lm_11 <- lmList(be ~ cosk_3 | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,be,candidate=cosk_3)]
FUN_DECOMPOSITION(lm_11)

