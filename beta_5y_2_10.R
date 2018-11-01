#################################
#### Exclude First Group
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")

da_beta_5y[,group_be:=ifelse(be<=quantile(be,0.1),1,0),by=ym]
da_beta_5y_2_10 <- da_beta_5y[group_be==0,.(ym,SecCode,be)]

save(da_beta_5y_2_10,file="C:/Users/Ding/Desktop/da_beta_5y_2_10.RData")