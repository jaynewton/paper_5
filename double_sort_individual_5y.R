#################################
#### Double Sort 
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_individual_m.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode")) 
da_m <- merge(da_m,da_individual_m,by=c("ym","SecCode")) 

da_m[,cor(be,individual)]

####
ym_index <- sort(unique(da_m$ym))
k_1 <- 5 # variable of interest
k_2 <- 3 # control variable

ret_p <- array(NA,c(length(ym_index),k_1,k_2)) # p denotes portfolio
# the k_1 corresponds to the number of groups of variable of interest
# the k_2 corresponds to the number of groups of control variable
# i,p and j corresponds to max(da_w$w), the k_1 and the k_2 below

for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  # Below is the control variable.
  da_sub <- da_sub[order(individual),]
  n_mid <- floor(nrow(da_sub)/k_2)
  if ((nrow(da_sub)-n_mid*(k_2-2))%%2==0){
    n_f <- (nrow(da_sub)-n_mid*(k_2-2))/2 # f denotes first, l denotes last
    n_l <- n_f
  } else {
    n_f <- (nrow(da_sub)-n_mid*(k_2-2)-1)/2
    n_l <- n_f+1
  }
  x <- seq(from=n_f,to=nrow(da_sub),by=n_mid)[1:(k_2-1)]
  x <- c(x,nrow(da_sub))
  da_sub$group_n1 <- cut(1:nrow(da_sub), c(0,x),labels = 1:k_2)
  for (j in 1:k_2) {
    da_ss <- da_sub[group_n1==j,] # ss denotes the subset of the subset
    # Below is variable of interest
    da_ss <- da_ss[order(be),]
    n_mid <- floor(nrow(da_ss)/k_1)
    if ((nrow(da_ss)-n_mid*(k_1-2))%%2==0){
      n_f <- (nrow(da_ss)-n_mid*(k_1-2))/2 # f denotes the first, l the denotes last
      n_l <- n_f
    } else {
      n_f <- (nrow(da_ss)-n_mid*(k_1-2)-1)/2
      n_l <- n_f+1
    }
    x <- seq(from=n_f,to=nrow(da_ss),by=n_mid)[1:(k_1-1)]
    x <- c(x,nrow(da_ss))
    da_ss$group_n2 <- cut(1:nrow(da_ss), c(0,x),labels = 1:k_1)
    for (p in 1:k_1) {
      ret_p[i,p,j] <- da_ss[group_n2==p,mean(ret_e)]
      #ret_p[i,p,j] <- da_ss[group_n2==p,weighted.mean(ret_e,size)]
    }
  }
}

ret_p_m <- matrix(NA,nrow=k_1+1,ncol=k_2+1)
colnames(ret_p_m) <- c(paste0("cv",1:k_2),"average") # cv denotes control variables
rownames(ret_p_m) <- c(paste0("voi",1:k_1),"h-l") # voi denotes variables of interest, here it's rsj
for (p in 1:k_1) {
  for (j in 1:k_2) {
    ret_p_m[p,j] <- mean(ret_p[,p,j],na.rm=T) # full sample
  }
}

ret_p_m[1:k_1,k_2+1] <- rowMeans(ret_p_m[1:k_1,1:k_2])
ret_p_m[k_1+1,] <- ret_p_m[k_1,]-ret_p_m[1,]
ret_p_m

#### Newey-West t statistic
t_nm <- vector(length=k_2+1)

## For the first five t values
ret_p_hl <- matrix(NA,nrow=length(ym_index),ncol=k_2)
ret_p_hl_sd <- vector(length=k_2)
for (j in 1:k_2) {
  ret_p_hl[,j] <- ret_p[,k_1,j]-ret_p[,1,j]
  ret_p_hl_sd[j] <- sd(ret_p_hl[,j],na.rm=T)
}
for (j in 1:k_2) {
  model_nm <- lm(ret_p_hl[,j] ~ 1)
  t_nm[j] <- coeftest(model_nm,vcov = NeweyWest(model_nm))[1,3]
}

## For the sixth t value
ret_p_hl_average <- rowMeans((ret_p[,k_1,1:k_2]))-rowMeans((ret_p[,1,1:k_2]))
model_nm <- lm(ret_p_hl_average ~ 1)
t_nm[k_2+1] <- coeftest(model_nm,vcov = NeweyWest(model_nm))[1,3]
t_nm

#### adjusted by FF3F
FF3F_nm <- copy(FF3F_A_nm)
ret_p_hl <- cbind(ret_p_hl,ret_p_hl_average)
ret_p_hl <- na.omit(as.data.table(ret_p_hl))
names(ret_p_hl) <- c(paste0("p",1:k_2),"average")
ret_p_hl$ym <- sort(unique(da_m$ym))
ret_p_hl <- merge(ret_p_hl,FF3F_nm,by="ym")

## Newey-West t statistic
ret_p_hl_FF3F <- matrix(NA,nrow=2,ncol=k_2+1) 
for (j in 1:(k_2+1)) { # the first column is the corresponding ym
  model_FF3F <- lm(ret_p_hl[[j+1]]~ret_p_hl[["mkt_e"]]+ret_p_hl[["smb"]]+ret_p_hl[["hml"]])
  ret_p_hl_FF3F[1,j] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
  ret_p_hl_FF3F[2,j] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
}
ret_p_hl_FF3F
coeftest(model_FF3F)[1,3] # t value of portfolio 5-1
