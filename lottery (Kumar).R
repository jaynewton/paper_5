#################################
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_6m.RData")

da_lottery_m <- merge(da_all_m[,.(ym,SecCode,clpr)],da_ivol_6m,by=c("ym","SecCode"))

da_lottery_m[,`:=`(percentile_ivol=percent_rank(ivol),
                   percentile_isk=percent_rank(isk)),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_lottery_m[,lottery:=(percentile_ivol+percentile_isk)/2]
da_lottery_m <- da_lottery_m[,.(ym,SecCode,lottery)]

save(da_lottery_m,file="C:/Users/Ding/Desktop/da_lottery_m.RData")

