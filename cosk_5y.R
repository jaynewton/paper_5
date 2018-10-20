load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/da_mkt_e_m_1995.RData")

da_cosk_m <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)]
da_cosk_m <- merge(da_cosk_m,da_mkt_e_m_1995,by="ym")

da_ym <- da_cosk_m[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_cosk_m <- merge(da_cosk_m,da_ym,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_cosk_m[,.N,by=SecCode][N>=60,SecCode]
da_cosk_m <- da_cosk_m[SecCode %in% selectedcode,]

#### Calculate the Total Skewness
da_cosk_m[,`:=`(intercept=1,mkt_e_2=mkt_e^2)]

#### Method 1: Use Matrix
# The speed is much faster.
# Use matrix rather than data.table if possible.

#### Ang et al. (2006)
FUN_COSK_1 <- function(da) {
  da <- na.omit(da)
  cosk <- ifelse(nrow(da)<50,as.numeric(NA),
                 mean((da[,1]-mean(da[,1]))*(da[,2]-mean(da[,2]))^2)/
                   (sqrt(mean((da[,1]-mean(da[,1]))^2))*mean((da[,2]-mean(da[,2]))^2)))
  return(cosk)
}

#### Harvey and Siddique (2000)
FUN_COSK_2 <- function(da) {
  da <- na.omit(da)
  if (nrow(da)<50) {
    cosk <- as.numeric(NA)
  } else {
    coef_vari <- (solve(t(da[,c(2,3)]) %*% da[,c(2,3)]) %*% t(da[,c(2,3)]) %*% da[,1])[,1]
    cosk <- mean((da[,1]-da[,c(2,3)] %*% coef_vari)*(da[,3]-mean(da[,3]))^2)/
      (sqrt(mean((da[,1]-da[,c(2,3)] %*% coef_vari)^2))*mean((da[,3]-mean(da[,3]))^2))
  }
  return(cosk)
}

#### Harvey and Siddique (2000)
FUN_COSK_3 <- function(da) {
  da <- na.omit(da)
  cosk <- ifelse(nrow(da)<50,as.numeric(NA),
                 (solve(t(da[,2:4]) %*% da[,2:4]) %*% 
                    t(da[,2:4]) %*% da[,1])[3,1])
  return(cosk)
}

# We require a stock must have at least 50-month trading record in past 60 months.
now()
da_cosk_m[,cosk_1:=rollapply(as.matrix(.SD),60,FUN_COSK_1,by=1,by.column=F,align="right",fill=NA),
        by=SecCode,.SDcols=c("ret_e_tm","mkt_e")]
da_cosk_m[,cosk_2:=rollapply(as.matrix(.SD),60,FUN_COSK_2,by=1,by.column=F,align="right",fill=NA),
          by=SecCode,.SDcols=c("ret_e_tm","intercept","mkt_e")]
da_cosk_m[,cosk_3:=rollapply(as.matrix(.SD),60,FUN_COSK_3,by=1,by.column=F,align="right",fill=NA),
          by=SecCode,.SDcols=c("ret_e_tm","intercept","mkt_e","mkt_e_2")]
now()
####

#### Method 2: Use data.table
# The speed is much lower since we have to state the class is data.table again and again.
FUN_COSK_1 <- function(da) {
  da <- na.omit(as.data.table(da))
  # The compatibility between data.table and rollapply is not good.
  # That's why we need to state the class of data is data.table.
  cosk <- ifelse(nrow(da)<50,as.numeric(NA),
                 da[,mean((ret_e_tm-mean(ret_e_tm))*(mkt_e-mean(mkt_e))^2)/
                      (sqrt(mean((ret_e_tm-mean(ret_e_tm))^2))*mean((mkt_e-mean(mkt_e))^2))])
  return(cosk)
}
# We require a stock must have at least 50-month trading record in past 60 months.
now()
da_cosk_m[,cosk:=rollapply(.SD,60,FUN_COSK_1,by=1,by.column=F,align="right",fill=NA),
          by=SecCode,.SDcols=c("ret_e_tm","mkt_e")]
now()
####

da_cosk_5y <- na.omit(da_cosk_m[,.(ym,SecCode,cosk_1,cosk_2,cosk_3)])

save(da_cosk_5y,file="C:/Users/Ding/Desktop/da_cosk_5y.RData")
