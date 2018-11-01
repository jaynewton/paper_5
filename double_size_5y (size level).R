#################################
#### Double Sort 
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")


# Note: The results for value weighted cases are not that desirable 
# when the control variable is mom.

da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode")) # for size

####
ym_index <- sort(unique(da_m$ym))
k <- 5 # 5*5 portfolios
ret_p <- array(NA,c(length(ym_index),k,k)) # p denotes portfolio
# the first k corresponds to the number of groups of variable of interest
# the second k corresponds to the number of groups of control variable
# i,p and j corresponds to max(da_w$w), the first k and the second k below

for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  # Below is the control variable.
  da_sub <- da_sub[order(size),]
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
  da_sub$group_n1 <- cut(1:nrow(da_sub), c(0,x),labels = 1:k)
  for (j in 1:k) {
    da_ss <- da_sub[group_n1==j,] # ss denotes the subset of the subset
    # Below is variable of interest
    da_ss <- da_ss[order(be),]
    n_mid <- floor(nrow(da_ss)/k)
    if ((nrow(da_ss)-n_mid*(k-2))%%2==0){
      n_f <- (nrow(da_ss)-n_mid*(k-2))/2 # f denotes the first, l the denotes last
      n_l <- n_f
    } else {
      n_f <- (nrow(da_ss)-n_mid*(k-2)-1)/2
      n_l <- n_f+1
    }
    x <- seq(from=n_f,to=nrow(da_ss),by=n_mid)[1:(k-1)]
    x <- c(x,nrow(da_ss))
    da_ss$group_n2 <- cut(1:nrow(da_ss), c(0,x),labels = 1:k)
    for (p in 1:k) {
      ret_p[i,p,j] <- da_ss[group_n2==p,mean(size)]
      #ret_p[i,p,j] <- da_ss[group_n2==p,weighted.mean(size,size)]
    }
  }
}

ret_p_m <- matrix(NA,nrow=k,ncol=k+1)
colnames(ret_p_m) <- c(paste0("cv",1:k),"average") # cv denotes control variables
rownames(ret_p_m) <- c(paste0("voi",1:k)) # voi denotes variables of interest
for (p in 1:k) {
  for (j in 1:k) {
    ret_p_m[p,j] <- mean(ret_p[,p,j],na.rm=T) # full sample
  }
}
ret_p_m[1:k,k+1] <- rowMeans(ret_p_m[1:k,1:k])
ret_p_m

