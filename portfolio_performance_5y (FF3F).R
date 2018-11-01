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

cor(ret_p[,.(ret_lh,mkt_e,smb,hml)])
cor(ret_p[,.(ret_lh,mkt_e,smb,hml)],method="spearman")

#1.连续数据，正态分布，线性关系，用pearson相关系数是最恰当，
#当然用spearman相关系数也可以，就是效率没有pearson相关系数高。
#2.上述任一条件不满足，就用spearman相关系数，不能用pearson相关系数。

#### cumulated value (English)
ret_plot <- melt(ret_p,id.vars="ym",
                 measure.vars=c("exp_cum_be","exp_cum_mkt_e","exp_cum_smb","exp_cum_hml"),
                 variable.name="strategies",value.name="cumulated_value")

p <- ggplot(ret_plot,aes(x=ym,y=cumulated_value,colour=strategies,linetype=strategies))
p+geom_line()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.title.x=element_blank())+
  ylab("Cumulative Values")+
  theme(axis.title=element_text(size=rel(1.8)),
        axis.text=element_text(size=rel(1.8)))+
  theme(legend.position=c(0.11,0.84),
        legend.background=element_rect(colour="black"),
        legend.text=element_text(size=rel(2)),
        legend.key.size=unit(1.7,'cm'))+
  scale_colour_grey(start=0.5,end=0,
                    guide=guide_legend(title=NULL),
                    labels=c("Beta","MKT","SMB","HML"))+
  #scale_colour_manual(values=c("red","blue","black"),
  #guide=guide_legend(title=NULL),
  #labels=c("Beta","MKT"))+
  scale_linetype_discrete(guide=guide_legend(title=NULL),
                          labels=c("Beta","MKT","SMB","HML"))
ggsave("C:/Users/Ding/Desktop/cumulated value.png",width=1.618*20,height=20,units="cm",dpi=500)
#ggsave("C:/Users/Ding/Desktop/cumulated value.pdf",width=1.618*20,height=20,units="cm",dpi=500)

#### cumulated value (Chinese)
ret_plot <- melt(ret_p,id.vars="ym",
                 measure.vars=c("exp_cum_be","exp_cum_mkt_e","exp_cum_smb","exp_cum_hml"),
                 variable.name="strategies",value.name="cumulated_value")

p <- ggplot(ret_plot,aes(x=ym,y=cumulated_value,colour=strategies,linetype=strategies))
p+geom_line()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.title.x=element_blank())+
  ylab("组合价值")+
  theme(axis.title=element_text(size=rel(1.8)),
        axis.text=element_text(size=rel(1.8)))+
  theme(legend.position=c(0.175,0.84),
        legend.background=element_rect(colour="black"),
        legend.text=element_text(size=rel(2)),
        legend.key.size=unit(1.7,'cm'))+
  scale_colour_grey(start=0.5,end=0,
                    guide=guide_legend(title=NULL),
                    labels=c("市场贝塔","市场超额收益率","SMB","HML"))+
  scale_linetype_discrete(guide=guide_legend(title=NULL),
                          labels=c("市场贝塔","市场超额收益率","SMB","HML"))
ggsave("C:/Users/Ding/Desktop/cumulated value.png",width=1.618*20,height=20,units="cm",dpi=500)

#### cumulated return (English)
ret_plot <- melt(ret_p,id.vars="ym",
                 measure.vars=c("cum_be","cum_mkt_e","cum_smb","cum_hml"),
                 variable.name="strategies",value.name="cumulated_return")

p <- ggplot(ret_plot,aes(x=ym,y=cumulated_return,colour=strategies,linetype=strategies))
p+geom_line()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.title.x=element_blank())+
  ylab("Cumulative Returns")+
  theme(axis.title=element_text(size=rel(1.8)),
        axis.text=element_text(size=rel(1.8)))+
  theme(legend.position=c(0.095,0.87),
        legend.background=element_rect(colour="black"),
        legend.text=element_text(size=rel(2)),
        legend.key.size=unit(1.7,'cm'))+
  scale_colour_grey(start=0.5,end=0,
                    guide=guide_legend(title=NULL),
                    labels=c("Beta","MKT","SMB","HML"))+
  scale_linetype_discrete(guide=guide_legend(title=NULL),
                          labels=c("Beta","MKT","SMB","HML"))
ggsave("C:/Users/Ding/Desktop/cumulated return.png",width=1.618*20,height=20,units="cm",dpi=500)
#ggsave("C:/Users/Ding/Desktop/cumulated return.pdf",width=1.618*20,height=20,units="cm",dpi=500)

#### cumulated return (Chinese)
ret_plot <- melt(ret_p,id.vars="ym",
                 measure.vars=c("cum_be","cum_mkt_e","cum_smb","cum_hml"),
                 variable.name="strategies",value.name="cumulated_return")

p <- ggplot(ret_plot,aes(x=ym,y=cumulated_return,colour=strategies,linetype=strategies))
p+geom_line()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.title.x=element_blank())+
  ylab("组合累计收益率")+
  theme(axis.title=element_text(size=rel(1.8)),
        axis.text=element_text(size=rel(1.8)))+
  theme(legend.position=c(0.16,0.87),
        legend.background=element_rect(colour="black"),
        legend.text=element_text(size=rel(2)),
        legend.key.size=unit(1.7,'cm'))+
  scale_colour_grey(start=0.5,end=0,
                    guide=guide_legend(title=NULL),
                    labels=c("市场贝塔","市场超额收益率","SMB","HML"))+
  scale_linetype_discrete(guide=guide_legend(title=NULL),
                          labels=c("市场贝塔","市场超额收益率","SMB","HML"))
ggsave("C:/Users/Ding/Desktop/cumulated return.png",width=1.618*20,height=20,units="cm",dpi=500)

#### Mean Return
ret_p[,sum(ret_lh)/nrow(ret_p)*12]
ret_p[,sum(mkt_e)/nrow(ret_p)*12]
ret_p[,sum(smb)/nrow(ret_p)*12]
ret_p[,sum(hml)/nrow(ret_p)*12]

#### Value at Risk(VaR) 
ret_p[,quantile(ret_lh,0.01)]
ret_p[,quantile(mkt_e,0.01)]
ret_p[,quantile(smb,0.01)]
ret_p[,quantile(hml,0.01)]

ret_p[,quantile(ret_lh,0.05)]
ret_p[,quantile(mkt_e,0.05)]
ret_p[,quantile(smb,0.05)]
ret_p[,quantile(hml,0.05)]

#### Expected Shortfall
ret_p[ret_lh<quantile(ret_lh,0.01),mean(ret_lh)]
ret_p[mkt_e<quantile(mkt_e,0.01),mean(mkt_e)]
ret_p[smb<quantile(smb,0.01),mean(smb)]
ret_p[hml<quantile(hml,0.01),mean(hml)]

ret_p[ret_lh<quantile(ret_lh,0.05),mean(ret_lh)]
ret_p[mkt_e<quantile(mkt_e,0.05),mean(mkt_e)]
ret_p[smb<quantile(smb,0.05),mean(smb)]
ret_p[hml<quantile(hml,0.05),mean(hml)]

#### Sharpe Ratio (Yearly)
ret_p[,mean(ret_lh)/sd(ret_lh)]*sqrt(nrow(ret_p)/nrow(ret_p)*12) 
ret_p[,mean(mkt_e)/sd(mkt_e)]*sqrt(nrow(ret_p)/nrow(ret_p)*12)
ret_p[,mean(smb)/sd(smb)]*sqrt(nrow(ret_p)/nrow(ret_p)*12) 
ret_p[,mean(hml)/sd(hml)]*sqrt(nrow(ret_p)/nrow(ret_p)*12)

