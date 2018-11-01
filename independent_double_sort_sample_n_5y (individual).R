#################################
#### Calculate the Mean Sample Number in Each Portfolio
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_individual_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

da_all_m <- da_all_m[,.(ym,SecCode,ret_e,size)]
da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.3333),1,
                                          ifelse(individual<=quantile(individual,0.6666),2,3)),by=ym]
da_beta_5y[,group_be:=ifelse(be<=quantile(be,0.2),1,
                             ifelse(be<=quantile(be,0.4),2,
                                    ifelse(be<=quantile(be,0.6),3,
                                           ifelse(be<=quantile(be,0.8),4,
                                                  5)))),by=ym]
da_m <- merge(da_all_m,da_individual_m,by=c("ym","SecCode")) 
da_m <- merge(da_m,da_beta_5y,by=c("ym","SecCode")) 

####
ym_index <- sort(unique(da_m$ym))
k_1 <- 5 
k_2 <- 3
ret_p <- array(NA,c(length(ym_index),k_1,k_2)) # p denotes portfolio
# the first k_1 corresponds to the number of groups of variable of interest
# the second k_2 corresponds to the number of groups of control variable
# i,p and j corresponds to 1:length(ym_index), the k_1 and the k_2 below

for (i in 1:length(ym_index)) {
  da_sample_n <- da_m[ym==ym_index[i],.(sample_n=.N),keyby=.(group_be,group_individual)]
  ret_p[i,,] <- as.matrix(dcast(da_sample_n, group_be ~ group_individual, value.var="sample_n")[,-"group_be"])
}

ret_p_m <- matrix(NA,nrow=k_1,ncol=k_2)
colnames(ret_p_m) <- c(paste0("cv",1:k_2)) # cv denotes control variables
rownames(ret_p_m) <- c(paste0("voi",1:k_1)) # voi denotes variables of interest, here it's rsj
for (p in 1:k_1) {
  for (j in 1:k_2) {
    ret_p_m[p,j] <- mean(ret_p[,p,j],na.rm=T) # full sample
  }
}
ret_p_m

