#################################
#### Portfolio Characteristics (in One Box) 
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")

da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
da_m <- merge(da_m,da_realized_m,by=c("ym","SecCode"))

#names(da_m)

ym_index <- sort(unique(da_m$ym))
k <- 5
y <- 3  # number of porfolio characteristics variables
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- array(NA,c(length(ym_index),k,y)) # vari_level denotes variable level
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
    vari_level[i,j,1] <- da_sub[group_n==j,mean(be)]
    vari_level[i,j,2] <- da_sub[group_n==j,mean(rvol)]
    vari_level[i,j,3] <- da_sub[group_n==j,mean(rsk)]
  }
}
vari_level_m <- matrix(NA,nrow=k,ncol=y) # m denotes mean 
for (j in 1:k) {
  for (p in 1:y) {
    vari_level_m[j,p] <- mean(vari_level[,j,p],na.rm=T)
  }
}
colnames(vari_level_m) <- c("be","rvol","rsk")
vari_level_m
