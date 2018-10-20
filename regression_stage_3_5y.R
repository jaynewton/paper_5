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

#skew(da_realized_m$rkt) # 10.50206
# That's why we should take log of rkt when it is used in the regression.

da_m_lm[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq))]
da_m_lm <- da_m_lm[order(ym,SecCode),]

#### The Regression Part
lm_fit_nw <- function(model,nv) { # nw denotes Newey-West HAC t statistic
  # nv denotes numbers of variables
  lm_sta <- matrix(NA,nrow=nv+1,ncol=2) # sta denotes statistics
  for (i in 1:(nv+1)) {
    model_nw <- lm(summary(model)$coefficients[,,i][,1] ~ 1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  return(lm_sta)
}

####
lm_1 <- lmList(be ~ max_ret | ym , data=da_m_lm)
lm_fit_nw(lm_1,1)
lm_2 <- lmList(be ~ size | ym , data=da_m_lm)
lm_fit_nw(lm_2,1)
lm_3 <- lmList(be ~ BM | ym , data=da_m_lm)
lm_fit_nw(lm_3,1)
lm_4 <- lmList(be ~ mom | ym , data=da_m_lm)
lm_fit_nw(lm_4,1)
lm_5 <- lmList(be ~ ret_e_tm | ym , data=da_m_lm)
lm_fit_nw(lm_5,1)
lm_6 <- lmList(be ~ illiq | ym , data=da_m_lm)
lm_fit_nw(lm_6,1)
lm_7 <- lmList(be ~ ivol | ym , data=da_m_lm)
lm_fit_nw(lm_7,1)
lm_8 <- lmList(be ~ isk | ym , data=da_m_lm)
lm_fit_nw(lm_8,1)
lm_9 <- lmList(be ~ cosk_1 | ym , data=da_m_lm)
lm_fit_nw(lm_9,1)
lm_10 <- lmList(be ~ cosk_2 | ym , data=da_m_lm)
lm_fit_nw(lm_10,1)
lm_11 <- lmList(be ~ cosk_3 | ym , data=da_m_lm)
lm_fit_nw(lm_11,1)

