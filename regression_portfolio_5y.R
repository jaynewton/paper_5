################################# 
#### Firm-Level Cross-Sectional Regression Based on Portfolios 
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

ym_index <- sort(unique(da_m_lm$ym))

k <- 50
#k <- 100
#k <- 200

da_m_lm_intermediate <- data.table() # p denotes portfolio
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m_lm[ym==ym_index[i],]
  da_sub <- da_sub[order(be),]
  n_mid <- floor(nrow(da_sub)/k)
  if ((nrow(da_sub)-n_mid*(k-2))%%2==0){
    n_f <- (nrow(da_sub)-n_mid*(k-2))/2 # f denotes first, l denotes last
    n_l <- n_f
  } else {
    n_f <- (nrow(da_sub)-n_mid*(k-2)-1)/2
    n_l <- n_f+1
  }
  x <- seq(from=n_f,to=nrow(da_sub),by=n_mid)[1:(k-1)]
  x <- c(x,nrow(da_sub))
  da_sub$group_n <- cut(1:nrow(da_sub), c(0,x),labels = 1:k)
  da_m_lm_intermediate <- rbind(da_m_lm_intermediate,da_sub)
}

da_p <- da_m_lm_intermediate[,lapply(.SD,mean),by=.(ym,group_n),
                             .SDcols=names(da_m_lm_intermediate)[!names(da_m_lm_intermediate) %in% c("ym","SecCode","group_n")]]
da_p_50 <- da_p
#da_p_100 <- da_p
#da_p_200 <- da_p

save(da_p_50,file="C:/Users/Ding/Desktop/da_p_50.RData")
#save(da_p_100,file="C:/Users/Ding/Desktop/da_p_100.RData")
#save(da_p_200,file="C:/Users/Ding/Desktop/da_p_200.RData")

################################# 
#### The Regression Part
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_p_50.RData")
#load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_p_100.RData")
#load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_p_200.RData")

da_p <- da_p_50
#da_p <- da_p_100
#da_p <- da_p_200

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
lm_1 <- lmList(ret_e ~ be | ym , data=da_p)
lm_fit_nw(lm_1,1) 
lm_2 <- lmList(ret_e ~ be+size+BM | ym , data=da_p)
lm_fit_nw(lm_2,3)
lm_3 <- lmList(ret_e ~ be+size+BM+mom | ym , data=da_p)
lm_fit_nw(lm_3,4)
lm_4 <- lmList(ret_e ~ be+size+BM+mom+ret_tm+illiq | ym , data=da_p)
lm_fit_nw(lm_4,6)
lm_5 <- lmList(ret_e ~ be+size+BM+mom+ret_tm+illiq+cosk_2 | ym , data=da_p)
lm_fit_nw(lm_5,7)

lm_6 <- lmList(ret_e ~ max_ret | ym , data=da_p)
lm_fit_nw(lm_6,1)
lm_7 <- lmList(ret_e ~ be+max_ret | ym , data=da_p)
lm_fit_nw(lm_7,2)
lm_8 <- lmList(ret_e ~ be+max_ret+size+BM | ym , data=da_p)
lm_fit_nw(lm_8,4)
lm_9 <- lmList(ret_e ~ be+max_ret+size+BM+mom | ym , data=da_p)
lm_fit_nw(lm_9,5)
lm_10 <- lmList(ret_e ~ be+max_ret+size+BM+mom+ret_tm+illiq | ym , data=da_p)
lm_fit_nw(lm_10,7)
lm_11 <- lmList(ret_e ~ be+max_ret+size+BM+mom+ret_tm+illiq+cosk_2 | ym , data=da_p)
lm_fit_nw(lm_11,8)
