library(Rmisc) # for multiplot

#################################
#### Short Is Allowed (Based on Simple Average)
load("F:/我的论文/第五篇/RData/da_all_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_m.RData") 

da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode"))
ym_index <- sort(unique(da_m$ym))

k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
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
    ret_p[i,j] <- da_sub[group_n==j,mean(ret_e)]
    #ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
  }
}

ret_p <- as.data.table(na.omit(ret_p))
ret_p$ret_lh <- ret_p[,1]-ret_p[,paste0("p",k),with=F] 
# ret_p_lh denotes low minus high portfolio returns
ret_p[,ym:=sort(unique(da_m$ym))]
ret_p[,ym:=ym+months(1)] # the right month strategies are applied

FF3F_m <- copy(FF3F_A_m)
ret_p <- merge(ret_p,FF3F_m,by="ym")
ret_p[,c(paste0("p",1:5)):=NULL]
ret_p <- ret_p[,.(ym,ret_lh,mkt_e,smb,hml)]

ret_p[,`:=`(cum_be=cumsum(ret_lh),cum_mkt_e=cumsum(mkt_e),
            cum_smb=cumsum(smb),cum_hml=cumsum(hml))]
ret_p[,`:=`(exp_cum_be=exp(cum_be),exp_cum_mkt_e=exp(cum_mkt_e),
            exp_cum_smb=exp(cum_smb),exp_cum_hml=exp(cum_hml))]

#### cumulated return (Chinese)
ret_plot <- melt(ret_p,id.vars="ym",
                 measure.vars=c("cum_be","cum_mkt_e"),
                 variable.name="strategies",value.name="cumulated_return")

p <- ggplot(ret_plot,aes(x=ym,y=cumulated_return,colour=strategies,linetype=strategies))
cumulated_return_p <- p+geom_line()+ theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.title.x=element_blank())+
  ylab("组合累计收益率")+
  theme(axis.title=element_text(size=rel(1.25)),
        axis.text=element_text(size=rel(1.25)))+
  theme(legend.position=c(0.21,0.8925),
        legend.background=element_rect(colour="black"),
        legend.text=element_text(size=rel(1.25)),
        legend.key.size=unit(0.5,'cm'))+
  scale_colour_grey(start=0.5,end=0,
                    guide=guide_legend(title=NULL),
                    labels=c("市场贝塔","市场超额收益率"))+
  scale_linetype_discrete(guide=guide_legend(title=NULL),
                          labels=c("市场贝塔","市场超额收益率"))
 

#### equal-weighted
p_ew <- cumulated_return_p
save(p_ew,file="C:/Users/Ding/Desktop/p_ew.RData")

#### value-weighted
p_vw <- cumulated_return_p
save(p_vw,file="C:/Users/Ding/Desktop/p_vw.RData")

#################################
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/p_ew.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/p_vw.RData")

multiplot(p_ew,p_vw,layout=matrix(c(1,2),ncol=2,byrow=T))
# 1250*392.5

#################################
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/p_ew.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/p_vw.RData")

multiplot(p_ew,p_vw,layout=matrix(c(1,2),ncol=1))
# 650*803

