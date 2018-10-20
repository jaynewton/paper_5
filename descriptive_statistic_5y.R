#################################
#### Descriptive Statistics (Except Realized Variables)
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/RData/da_ivol_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_cosk_5y.RData")

da_m <- NULL
da_m <- merge(da_all_m,da_beta_5y,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_mom_m,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_ivol_m,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_cosk_5y,keyby=c("ym","SecCode"))

cor(da_m[,.(be,ivol,isk,max_ret,size,BM,mom,ret_tm,illiq,cosk_2)],method="pearson")
#cor(da_m[,.(be,ivol,isk,max_ret,size,BM,mom,ret_tm,illiq,cosk_2)],method="spearman")

ds_2 <- describe(da_m[,.(be,ivol,isk,max_ret,size,BM,mom,ret_tm,illiq,cosk_2)])[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_2[,"kurtosis"] <- ds_2[,"kurtosis"]+3
#ds_2
format(ds_2,digits=3)

#################################
#### Descriptive Statistics (Realized Variables)
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")

da_m <- NULL
da_m <- merge(da_all_m,da_beta_5y,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_realized_m,keyby=c("ym","SecCode"))


cor(da_m[,.(be,rvol,rsk)],method="pearson")
#cor(da_m[,.(be,rvol,rsk)],method="spearman")

ds_2 <- describe(da_m[,.(be,rvol,rsk)])[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_2[,"kurtosis"] <- ds_2[,"kurtosis"]+3
#ds_2
format(ds_2,digits=3)

