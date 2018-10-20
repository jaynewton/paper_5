#################################
#### Load Monthly Risk-Free Rate
da_rf_m <- read.csv("F:/我的论文/第五篇/RData/risk_free_m.csv",header=T,stringsAsFactors=F)

da_rf_m <- as.data.table(da_rf_m)
names(da_rf_m) <- c("TDate","y","m","rf")
da_rf_m[,ym:=ymd(paste0(y,"-",m,"-01"))]
da_rf_m[,rf_inno:=c(NA,diff(rf))]
da_rf_m <- na.omit(da_rf_m[,.(ym,rf,rf_inno)])

save(da_rf_m,file="C:/Users/Ding/Desktop/da_rf_m.RData")

#################################
load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/da_rf_m.RData")

da_funding <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)] # funding denotes funding liquidity
da_funding <- merge(da_funding,da_rf_m,by="ym")
da_funding <- da_funding[order(SecCode,ym),] 

da_allmonth <- da_funding[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_funding <- merge(da_funding,da_allmonth,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_funding[,.N,by=SecCode][N>=60,SecCode]
da_funding <- da_funding[SecCode %in% selectedcode,]

da_funding[,intercept:=1,]
da_funding <- da_funding[,.(SecCode,ym,ret_e_tm,intercept,rf,rf_inno)]

now()
FUN_BE <- function(da) {
  da <- na.omit(da)
  return(ifelse(nrow(da)<50,as.numeric(NA),
                (solve(t(da[,c(2,3)]) %*% da[,c(2,3)]) %*% 
                   t(da[,c(2,3)]) %*% da[,1])[2,1]))
}

da_funding[,funding_1:=rollapply(as.matrix(.SD),60,FUN = FUN_BE,by=1,
                                 by.column = FALSE,align = "right", fill = NA),
           by=SecCode,.SDcols=c("ret_e_tm","intercept","rf")]

da_funding[,funding_2:=rollapply(as.matrix(.SD),60,FUN = FUN_BE,by=1,
                                 by.column = FALSE,align = "right", fill = NA),
           by=SecCode,.SDcols=c("ret_e_tm","intercept","rf_inno")]
now()

da_funding <- na.omit(da_funding)
da_funding <- da_funding[order(ym,SecCode),]

da_funding_5y <- da_funding[,.(ym,SecCode,funding_1,funding_2)] 

save(da_funding_5y,file="C:/Users/Ding/Desktop/da_funding_5y.RData")

