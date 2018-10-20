#################################
#### Time-Varying Premium
#### Method 1: Regression
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
da_business <- read.csv("F:/我的论文/第三篇/RData/business_index.csv")

da_business <- as.data.table(da_business)
names(da_business) <- c("TDate","business")
da_business[,TDate:=ymd(TDate)]
da_business[,`:=`(y=lubridate::year(TDate), 
                  m=lubridate::month(TDate))]
da_business[,ym:=ymd(paste0(y,"-",m,"-01"))]

#### FF3F is not controlled
da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))

lm_be <- lmList(ret_e ~ be | ym , data=da_m)
da_premium_be <- data.table(ym=ymd(names(summary(lm_be)$coefficients[,,2][,1])),
                           premium_be=as.numeric(summary(lm_be)$coefficients[,,2][,1]))
da_premium_be[,premium_be:=-premium_be]

da_premium_be <- merge(da_premium_be,da_business,by="ym")

model_nw_be <- lm(premium_be ~ business,data=da_premium_be)
summary(model_nw_be)$coefficients
coeftest(model_nw_be,vcov = NeweyWest(model_nw_be))[2,3]

#### Method 2: Two states and spreads 
#### Single Sort
#### Not Adjusted by FF3F
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
da_business <- read.csv("F:/我的论文/第三篇/RData/business_index.csv")

da_business <- as.data.table(da_business)
names(da_business) <- c("TDate","business")
da_business[,TDate:=ymd(TDate)]
da_business[,`:=`(y=lubridate::year(TDate), 
                  m=lubridate::month(TDate))]
da_business[,ym:=ymd(paste0(y,"-",m,"-01"))]

da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
da_m <- merge(da_m,da_business,by="ym")
da_m <- da_m[order(ym,SecCode),]
#median(da_m$mkt_e) 
da_m[,business_high:=ifelse(business>median(business),1,0)]
da_m_1 <- da_m[business_high==1,]
da_m_2 <- da_m[business_high==0,]
da_m <- NULL

####
da_m <- da_m_1
#da_m <- da_m_2

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

