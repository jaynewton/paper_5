#################################
#### Time-Varying Premium
#### Method 1: Regression
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")

da_all_m <- merge(da_all_m,da_beta_5y)

#### FF3F is not controlled
lm_be <- lmList(ret_e ~ be | ym , data=da_all_m)
da_premium_be <- data.table(ym=ymd(names(summary(lm_be)$coefficients[,,2][,1])),
                             premium_be=as.numeric(summary(lm_be)$coefficients[,,2][,1]))
#class(da_premium_be[,ym]) # "Date"
da_premium_be[,premium_be:=-premium_be]
# We change the sign to make it easy to explain the relationship between variables.

lm_max <- lmList(ret_e ~ max_ret | ym , data=da_all_m)
da_premium_max <- data.table(ym=ymd(names(summary(lm_max)$coefficients[,,2][,1])),
                             premium_max=as.numeric(summary(lm_max)$coefficients[,,2][,1]))
da_premium_max[,premium_max:=-premium_max]

da_premium <- merge(da_premium_be,da_premium_max,by="ym")
model <- lm(premium_be ~ premium_max,data=da_premium)
summary(model)
coeftest(model,vcov = NeweyWest(model))[2,3]

da_premium_max_5y <- da_premium[,.(ym,premium_max)]
save(da_premium_max_5y,file="C:/Users/Ding/Desktop/da_premium_max_5y.RData")

#################################
#### Method 2: Two states and spreads 
#### Single Sort
#### Not Adjusted by FF3F
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_premium_max_5y.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")
load("F:/我的论文/第五篇/RData/da_fmax_nm.RData")

da_premium <- copy(da_premium_max_5y)

da_premium[,max_high:=ifelse(premium_max>=median(premium_max),1,0)]
da_premium <- da_premium[,.(ym,max_high)]
da_all_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
da_all_m <- merge(da_all_m,da_premium,by="ym")
da_all_m <- da_all_m[order(ym,SecCode),]
#median(da_all_m[,premium_max]) 

da_all_m_1 <- da_all_m[max_high==1,]
da_all_m_2 <- da_all_m[max_high==0,]

####
da_m <- da_all_m_1
#da_m <- da_all_m_2

####
ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(sort(unique(da_m$ym))),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(sort(unique(da_m$ym))),ncol=k) 
# vari_level denotes variable level
for (i in 1:length(sort(unique(da_m$ym)))) {
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
    ret_p[i,j] <- da_sub[group_n==j,mean(ret_e)]
    # This part we do not need to take risk-free rate into consideration 
    #ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
    vari_level[i,j] <- da_sub[group_n==j,mean(be)]
    #vari_level[i,j] <- da_sub[group_n==j,weighted.mean(be,size)]
  }
}

ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
vari_level_m <- colMeans(vari_level,na.rm=T)
output <- cbind(vari_level_m,ret_p_m) 
output
result <- c(output[,2],hml=output[k,2]-output[1,2])
result

## Newey-West t statistic
ret_p_hl <- ret_p[,k]-ret_p[,1] # ret_p_hl denotes high minus low portfolio returns
model_nw <- lm(ret_p_hl ~ 1)
coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]

#### Adjusted by FF3F
FF3F_nm <- copy(FF3F_A_nm)
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,FF3F_nm,by="ym") 
## Newey-West t statistic
ret_p_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the ym
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

#################################
#### Adjusted by FMAX
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,da_fmax_nm,by="ym")

## Newey-West t statistic
ret_p_fmax <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the ym
  model_fmax <- lm(ret_p[[i+1]]~ret_p[,fmax]) 
  # ret[,i+1] is wrong. See 1.5 of Frequently Asked Questions about data.table
  ret_p_fmax[1,i] <- coeftest(model_fmax,vcov=NeweyWest(model_fmax))[1,1]
  ret_p_fmax[2,i] <- coeftest(model_fmax,vcov=NeweyWest(model_fmax))[1,3]
}
ret_p_hl <- ret_p[[k+1]]-ret_p[[2]] 
model_fmax <- lm(ret_p_hl~ret_p[,fmax])
ret_p_fmax[1,k+1] <- coeftest(model_fmax,vcov=NeweyWest(model_fmax))[1,1]
ret_p_fmax[2,k+1] <- coeftest(model_fmax,vcov=NeweyWest(model_fmax))[1,3]
ret_p_fmax

