#################################
#### Monthly BAB
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")

da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
da_m[,group_be:=ifelse(be>median(be),"h","l"),by=ym]

da_be_h <- da_m[group_be=="h",.(be_h=weighted.mean(ret_e,size)),by=ym]
da_be_l <- da_m[group_be=="l",.(be_l=weighted.mean(ret_e,size)),by=ym]

da_bab_nm_5y <- merge(da_be_h,da_be_l,by="ym") # bab denotes betting against beta
da_bab_nm_5y[,bab:=be_l-be_h]
da_bab_nm_5y <- da_bab_nm_5y[,.(ym,bab)]

#da_bab_nm_5y[,mean(bab)]*12

da_bab_m_5y <- copy(da_bab_nm_5y)
da_bab_m_5y[,ym:=ym+months(1)]

save(da_bab_m_5y,file="C:/Users/Ding/Desktop/da_bab_m_5y.RData")
save(da_bab_nm_5y,file="C:/Users/Ding/Desktop/da_bab_nm_5y.RData")

