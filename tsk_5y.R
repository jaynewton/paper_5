################################# 
load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/da_mkt_e_m_1995.RData")

da_tsk_m <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)]
da_tsk_m <- merge(da_tsk_m,da_mkt_e_m_1995,by="ym")

da_ym <- da_tsk_m[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_tsk_m <- merge(da_tsk_m,da_ym,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_tsk_m[,.N,by=SecCode][N>=60,SecCode]
da_tsk_m <- da_tsk_m[SecCode %in% selectedcode,]

#### Calculate the Total Skewness
#Method 1:
FUN_TSK <- function(vari) { # vari denotes variable
  return(ifelse((60-length(which(is.na(vari))))<50,as.numeric(NA),skew(vari,na.rm=T)))
}

#Method 2:
#FUN_TSK <- function(vari) {
#  vari <- na.omit(vari)
#  return(ifelse(length(vari)<50,as.numeric(NA),skew(vari,na.rm=T)))
#}

# We require a stock must have at least 50-month trading record in past 60 months.
now()
da_tsk_m[,tsk:=rollapply(ret_e_tm,60,FUN_TSK,by=1,align="right",fill=NA),by=SecCode]
now()

da_tsk_5y <- na.omit(da_tsk_m[,.(ym,SecCode,tsk)])

save(da_tsk_5y,file="C:/Users/Ding/Desktop/da_tsk_5y.RData")



