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

#names(da_m_lm)

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
lm_5 <- lmList(ret_e ~ be+size+BM+mom+ret_tm+illiq+cosk_2 | ym , data=da_m_lm)
lm_fit_nw(lm_5,7)

lm_6 <- lmList(ret_e ~ max_ret | ym , data=da_m_lm)
lm_fit_nw(lm_6,1)
lm_7 <- lmList(ret_e ~ be+max_ret | ym , data=da_m_lm)
lm_fit_nw(lm_7,2)
lm_8 <- lmList(ret_e ~ be+max_ret+size+BM | ym , data=da_m_lm)
lm_fit_nw(lm_8,4)
lm_9 <- lmList(ret_e ~ be+max_ret+size+BM+mom | ym , data=da_m_lm)
lm_fit_nw(lm_9,5)
lm_10 <- lmList(ret_e ~ be+max_ret+size+BM+mom+ret_tm+illiq | ym , data=da_m_lm)
lm_fit_nw(lm_10,7)
lm_11 <- lmList(ret_e ~ be+max_ret+size+BM+mom+ret_tm+illiq+cosk_2 | ym , data=da_m_lm)
lm_fit_nw(lm_11,8)

lm_12 <- lmList(ret_e ~ ivol | ym , data=da_m_lm)
lm_fit_nw(lm_12,1)
lm_13 <- lmList(ret_e ~ be+ivol | ym , data=da_m_lm)
lm_fit_nw(lm_13,2)
lm_14 <- lmList(ret_e ~ be+ivol+size+BM | ym , data=da_m_lm)
lm_fit_nw(lm_14,4)
lm_15 <- lmList(ret_e ~ be+ivol+size+BM+mom | ym , data=da_m_lm)
lm_fit_nw(lm_15,5)
lm_16 <- lmList(ret_e ~ be+ivol+size+BM+mom+ret_tm+illiq | ym , data=da_m_lm)
lm_fit_nw(lm_16,7)
lm_17 <- lmList(ret_e ~ be+ivol+size+BM+mom+ret_tm+illiq+cosk_2 | ym , data=da_m_lm)
lm_fit_nw(lm_17,8)

lm_18 <- lmList(ret_e ~ isk | ym , data=da_m_lm)
lm_fit_nw(lm_18,1)
lm_19 <- lmList(ret_e ~ be+isk | ym , data=da_m_lm)
lm_fit_nw(lm_19,2)
lm_20 <- lmList(ret_e ~ be+isk+size+BM | ym , data=da_m_lm)
lm_fit_nw(lm_20,4)
lm_21 <- lmList(ret_e ~ be+isk+size+BM+mom | ym , data=da_m_lm)
lm_fit_nw(lm_21,5)
lm_22 <- lmList(ret_e ~ be+isk+size+BM+mom+ret_tm+illiq | ym , data=da_m_lm)
lm_fit_nw(lm_22,7)
lm_23 <- lmList(ret_e ~ be+isk+size+BM+mom+ret_tm+illiq+cosk_2 | ym , data=da_m_lm)
lm_fit_nw(lm_23,8)

lm_24 <- lmList(ret_e ~ cosk_1 | ym , data=da_m_lm)
lm_fit_nw(lm_24,1)
lm_25 <- lmList(ret_e ~ cosk_2 | ym , data=da_m_lm)
lm_fit_nw(lm_25,1)
lm_26 <- lmList(ret_e ~ cosk_3 | ym , data=da_m_lm)
lm_fit_nw(lm_26,1)
lm_27 <- lmList(ret_e ~ be+cosk_1 | ym , data=da_m_lm)
lm_fit_nw(lm_27,2)
lm_28 <- lmList(ret_e ~ be+cosk_2 | ym , data=da_m_lm)
lm_fit_nw(lm_28,2)
lm_29 <- lmList(ret_e ~ be+cosk_3 | ym , data=da_m_lm)
lm_fit_nw(lm_29,2)

cor(da_m_lm[,.(isk,cosk_1,cosk_2,cosk_3)])


