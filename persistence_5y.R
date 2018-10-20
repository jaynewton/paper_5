#################################
#### Persistence of Variables of Interest
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")

da_m <- na.omit(merge(da_all_m,da_beta_5y))
da_nm <- da_m[,.(SecCode=SecCode,ym=ym-months(1),be_nm=be)]
#da_nm <- da_m[,.(SecCode=SecCode,ym=ym-months(2),be_nm=be)]
#da_nm <- da_m[,.(SecCode=SecCode,ym=ym-months(3),be_nm=be)]
#da_nm <- da_m[,.(SecCode=SecCode,ym=ym-months(6),be_nm=be)]
#da_nm <- da_m[,.(SecCode=SecCode,ym=ym-months(12),be_nm=be)]
#da_nm <- da_m[,.(SecCode=SecCode,ym=ym-months(36),be_nm=be)]
#da_nm <- da_m[,.(SecCode=SecCode,ym=ym-months(60),be_nm=be)]
da_m <- merge(da_m,da_nm,by=c("SecCode","ym"))

#################################
#### Transition Matrix
#### Single Sort Based on This month's Variable of Interest
FUN_GROUP <- function(da,variable_name,k,ymn) {
  # k corresponds to the number of portfolios
  # ymn corresponds to the year-month number.
  # 0 denotes this month and 1 denotes next month. 
  cutpoint <- seq(0,1,length.out=k+1)
  variable_quantile <- as.numeric(quantile(da[[variable_name]],cutpoint))
  da[da[[variable_name]]==variable_quantile[1],paste0("group_n_",ymn):=0L,by=ym]
  for (i in 1:k) {
    da[da[[variable_name]]>variable_quantile[i] & 
         da[[variable_name]]<=variable_quantile[i+1],
       paste0("group_n_",ymn):=i,by=ym]
  }
}

#################################
# the proportion of stocks with the top 20%(10%,5%) be value this month
# still staying in the top 20% next month

## top 20%
da_transition_1 <- da_m
k <- 5
FUN_GROUP(da_transition_1,"be",k,0)
FUN_GROUP(da_transition_1,"be_nm",k,1)
# the next month
transition <- matrix(NA,nrow=k,ncol=k)
for (i in 1:k) {
  for (j in 1:k) {
    transition[i,j] <- da_transition_1[,sum(group_n_0==i & group_n_1==j,na.rm=T)/
                                         sum(group_n_0==i,na.rm=T)]
  }
}
transition

## top 10% 
da_transition_2 <- da_m
k <-10
FUN_GROUP(da_transition_2,"be",k,0)
FUN_GROUP(da_transition_2,"be_nm",k,1)
# the next month
transition <- matrix(NA,nrow=k,ncol=k)
for (i in 1:k) {
  for (j in 1:k) {
    transition[i,j] <- da_transition_2[,sum(group_n_0==i & group_n_1==j,na.rm=T)/
                                         sum(group_n_0==i,na.rm=T)]
  }
}
#transition
transition[,c(1,2,9,10)]
da_transition_2[,sum(group_n_0==k & group_n_1 %in% c(k-1,k),na.rm=T)/
                  sum(group_n_0==k,na.rm=T)]

## top 5%
da_transition_3 <- da_m
k <-20
FUN_GROUP(da_transition_3,"be",k,0)
FUN_GROUP(da_transition_3,"be_nm",k,1)
# the next month
transition <- matrix(NA,nrow=k,ncol=k)
da_transition_3[,sum(group_n_0==k & group_n_1 %in% c(k-3,k-2,k-1,k),na.rm=T)/
                  sum(group_n_0==k,na.rm=T)]

#################################
#### Using Regression Method

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

lm_be_1 <- lmList(be_nm ~ be | ym , data=da_m)
lm_fit_nw(lm_be_1,1)

