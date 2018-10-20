################################# 
#### Firm-Level Cross-Sectional Regression 
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/daily data in one year/RData/da_ivol_y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")

da_m_lm <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_ivol_y,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_mom_m,by=c("ym","SecCode"))

#skew(da_realized_m$rkt) # 10.50206
# That's why we should take log of rkt when it is used in the regression.

da_m_lm[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),ivol=log(ivol))]
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
  print(lm_sta)
  print(mean(as.numeric(unlist(summary(model)$adj.r.squared))))
}

####  
lm_1 <- lmList(ret_e ~ be | ym , data=da_m_lm)
lm_fit_nw(lm_1,1) 
lm_2 <- lmList(ret_e ~ be+size+BM | ym , data=da_m_lm)
lm_fit_nw(lm_2,3)
lm_3 <- lmList(ret_e ~ be+size+BM+mom | ym , data=da_m_lm)
lm_fit_nw(lm_3,4)
lm_4 <- lmList(ret_e ~ be+size+BM+mom+ret_tm+illiq | ym , data=da_m_lm)
lm_fit_nw(lm_4,6)

lm_5 <- lmList(ret_e ~ ivol | ym , data=da_m_lm)
lm_fit_nw(lm_5,1)
lm_6 <- lmList(ret_e ~ be+ivol | ym , data=da_m_lm)
lm_fit_nw(lm_6,2)
lm_7 <- lmList(ret_e ~ be+ivol+size+BM | ym , data=da_m_lm)
lm_fit_nw(lm_7,4)
lm_8 <- lmList(ret_e ~ be+ivol+size+BM+mom | ym , data=da_m_lm)
lm_fit_nw(lm_8,5)
lm_9 <- lmList(ret_e ~ be+ivol+size+BM+mom+ret_tm+illiq | ym , data=da_m_lm)
lm_fit_nw(lm_9,7)

lm_10 <- lmList(ret_e ~ isk | ym , data=da_m_lm)
lm_fit_nw(lm_10,1)
lm_11 <- lmList(ret_e ~ be+isk | ym , data=da_m_lm)
lm_fit_nw(lm_11,2)
lm_12 <- lmList(ret_e ~ be+isk+size+BM | ym , data=da_m_lm)
lm_fit_nw(lm_12,4)
lm_13 <- lmList(ret_e ~ be+isk+size+BM+mom | ym , data=da_m_lm)
lm_fit_nw(lm_13,5)
lm_14 <- lmList(ret_e ~ be+isk+size+BM+mom+ret_tm+illiq | ym , data=da_m_lm)
lm_fit_nw(lm_14,7)

