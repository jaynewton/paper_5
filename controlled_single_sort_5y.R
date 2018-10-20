#################################
#### Single Sort with Control
#### The Regression Part
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/RData/da_ivol_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_cosk_5y.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

####
da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
# for max_ret, size, BM, ret_tm, illiq and clpr

#da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
#da_m <- merge(da_m,da_mom_m,by=c("ym","SecCode")) # for mom

#da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
#da_m <- merge(da_m,da_ivol_m,by=c("ym","SecCode")) # for ivol and isk

#da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
#da_m <- merge(da_m,da_realized_m,by=c("ym","SecCode")) # for rvol and rsk

#da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
#da_m <- merge(da_m,da_cosk_5y,by=c("ym","SecCode")) # for cosk_1, cosk_2 and cosk_3

####
da_m[,be_max_ret:=summary(lm(be ~ max_ret))$residuals,by=ym]
#da_m[,be_size:=summary(lm(be ~ size))$residuals,by=ym]
#da_m[,be_BM:=summary(lm(be ~ BM))$residuals,by=ym]
#da_m[,be_mom:=summary(lm(be ~ mom))$residuals,by=ym]
#da_m[,be_ret_tm:=summary(lm(be ~ ret_tm))$residuals,by=ym]
#da_m[,be_illiq:=summary(lm(be ~ illiq))$residuals,by=ym]
#da_m[,be_ivol:=summary(lm(be ~ ivol))$residuals,by=ym]
#da_m[,be_isk:=summary(lm(be ~ isk))$residuals,by=ym]
#da_m[,be_rvol:=summary(lm(be ~ rvol))$residuals,by=ym]
#da_m[,be_rsk:=summary(lm(be ~ rsk))$residuals,by=ym]
#da_m[,be_cosk_1:=summary(lm(be ~ cosk_1))$residuals,by=ym]
#da_m[,be_cosk_2:=summary(lm(be ~ cosk_2))$residuals,by=ym]
#da_m[,be_cosk_3:=summary(lm(be ~ cosk_3))$residuals,by=ym]
#da_m[,be_clpr:=summary(lm(be ~ clpr))$residuals,by=ym]

#### Not Adjusted by FF3F
ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  da_sub <- da_sub[order(be_max_ret),]
  #da_sub <- da_sub[order(be_size),]
  #da_sub <- da_sub[order(be_BM),]
  #da_sub <- da_sub[order(be_mom),]
  #da_sub <- da_sub[order(be_ret_tm),]
  #da_sub <- da_sub[order(be_illiq),]
  #da_sub <- da_sub[order(be_ivol),]
  #da_sub <- da_sub[order(be_isk),]
  #da_sub <- da_sub[order(be_rvol),]
  #da_sub <- da_sub[order(be_rsk),]
  #da_sub <- da_sub[order(be_cosk_1),]
  #da_sub <- da_sub[order(be_cosk_2),]
  #da_sub <- da_sub[order(be_cosk_3),]
  #da_sub <- da_sub[order(be_clpr),]
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
    ret_p[i,j] <- da_sub[group_n==j,mean(ret_e)]
    # This part we do not need to take risk-free rate into consideration
    #ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
  }
}

ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
c(ret_p_m,hl=as.numeric(ret_p_m[k]-ret_p_m[1]))

## Newey-West t statistic
ret_p_hl <- ret_p[,k]-ret_p[,1] # ret_p_hl denotes high minus low portfolio returns
model_nw <- lm(ret_p_hl ~ 1) # nw denotes Newey-West
coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]

#### Adjusted by FF3F
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,FF3F_A_nm,by="ym")
## Newey-West t statistic
ret_p_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the week
  model_FF3F <- lm(ret_p[[i+1]]~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml]) 
  # ret[,i+1] is wrong. See 1.5 of Frequently Asked Questions about data.table
  ret_p_FF3F[1,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
  ret_p_FF3F[2,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
}
ret_p_hl <- ret_p[[k+1]]-ret_p[[2]] 
model_FF3F <- lm(ret_p_hl~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml])
ret_p_FF3F[1,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
ret_p_FF3F[2,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
ret_p_FF3F

