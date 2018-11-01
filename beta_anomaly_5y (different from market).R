#################################
#### Single Sort
#### Not Adjusted by FF3F
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")
load("F:/我的论文/第五篇/RData/da_fmax_nm.RData")

da_ret_mean_m <- da_all_m[,.(ret_ew=mean(ret_e),ret_vw=weighted.mean(ret_e,size)),by=ym]

da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))

ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
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
  for (j in 1:k) {
    #ret_p[i,j] <- da_sub[group_n==j,mean(ret_e)]
    # This part we do not need to take risk-free rate into consideration 
    ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
  }
}

da_ret_m <- cbind(da_ret_mean_m,ret_p)

#### equal-weighted
da_ret_m[,mean(p1-ret_ew)]
model_1_ew <- da_ret_m[,lm(I(p1-ret_ew)~1)]
coeftest(model_1_ew,vcov = NeweyWest(model_1_ew))[1,3]

da_ret_m[,mean(p5-ret_ew)]
model_5_ew <- da_ret_m[,lm(I(p5-ret_ew)~1)]
coeftest(model_5_ew,vcov = NeweyWest(model_5_ew))[1,3]

#### value-weighted
da_ret_m[,mean(p1-ret_vw)]
model_1_vw <- da_ret_m[,lm(I(p1-ret_vw)~1)]
coeftest(model_1_vw,vcov = NeweyWest(model_1_vw))[1,3]

da_ret_m[,mean(p5-ret_vw)]
model_5_vw <- da_ret_m[,lm(I(p5-ret_vw)~1)]
coeftest(model_5_vw,vcov = NeweyWest(model_5_vw))[1,3]

#################################
#### Adjusted by FF3F
rm(ret_p)
da_ret_m <- merge(da_ret_m,FF3F_A_nm,by="ym") 

#### equal-weighted
model_1_ew_FF3F <- da_ret_m[,lm(I(p1-ret_ew)~mkt_e+smb+hml)]
coeftest(model_1_ew_FF3F,vcov = NeweyWest(model_1_ew_FF3F))[1,1]
coeftest(model_1_ew_FF3F,vcov = NeweyWest(model_1_ew_FF3F))[1,3]

model_5_ew_FF3F <- da_ret_m[,lm(I(p5-ret_ew)~mkt_e+smb+hml)]
coeftest(model_5_ew_FF3F,vcov = NeweyWest(model_5_ew_FF3F))[1,1]
coeftest(model_5_ew_FF3F,vcov = NeweyWest(model_5_ew_FF3F))[1,3]

#### value-weighted
model_1_vw_FF3F <- da_ret_m[,lm(I(p1-ret_vw)~mkt_e+smb+hml)]
coeftest(model_1_vw_FF3F,vcov = NeweyWest(model_1_vw_FF3F))[1,1]
coeftest(model_1_vw_FF3F,vcov = NeweyWest(model_1_vw_FF3F))[1,3]

model_5_vw_FF3F <- da_ret_m[,lm(I(p5-ret_vw)~mkt_e+smb+hml)]
coeftest(model_5_vw_FF3F,vcov = NeweyWest(model_5_vw_FF3F))[1,1]
coeftest(model_5_vw_FF3F,vcov = NeweyWest(model_5_vw_FF3F))[1,3]

