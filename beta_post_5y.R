#################################
load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/da_mkt_e_m_1995.RData")

da_beta <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)]
da_beta <- merge(da_beta,da_mkt_e_m_1995,by="ym")
da_beta <- da_beta[order(SecCode,ym),] 

da_allmonth <- da_beta[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_beta <- merge(da_beta,da_allmonth,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_beta[,.N,by=SecCode][N>=60,SecCode]
da_beta <- da_beta[SecCode %in% selectedcode,]

da_beta[,intercept:=1,]
#da_beta <- da_beta[,.(SecCode,ym,ret_e_tm,intercept,mkt_e)]

now()
FUN_BE <- function(da) {
  da <- na.omit(da)
  return(ifelse(nrow(da)<50,as.numeric(NA),
                (solve(t(da[,c(2,3)]) %*% da[,c(2,3)]) %*% 
                   t(da[,c(2,3)]) %*% da[,1])[2,1]))
}
da_beta[,be:=rollapply(as.matrix(.SD),60,FUN = FUN_BE,by=1,
                       by.column = FALSE,align = "left", fill = NA),
        by=SecCode,.SDcols=c("ret_e_tm","intercept","mkt_e")]
now()

da_beta <- na.omit(da_beta)
da_beta <- da_beta[order(ym,SecCode),]

da_beta_5y <- da_beta[ym>=ymd("2000-1-1"),.(ym,SecCode,be)] # make full use of renewed information

da_beta_post_5y <- copy(da_beta_5y)

save(da_beta_post_5y,file="C:/Users/Ding/Desktop/da_beta_post_5y.RData")

