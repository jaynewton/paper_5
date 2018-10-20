################################# 
#### Firm-Level Cross-Sectional Regression  
#### Compute the Median Data Frame
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/RData/da_ind.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_ivol_m.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_cosk_5y.RData")

# Note: In most cases, a company's industry code doesn't change month by month.
# So it's OK to use this month's industry code (as we do here).
# A similar result will be obtained from the next month's industry code.
da_m_ind <- merge(da_all_m,da_ind,by=c("ym","SecCode"))
da_m_ind <- merge(da_m_ind,da_beta_5y,by=c("ym","SecCode"))
da_m_ind <- merge(da_m_ind,da_ivol_m,by=c("ym","SecCode"))
da_m_ind <- merge(da_m_ind,da_mom_m,by=c("ym","SecCode"))
da_m_ind <- merge(da_m_ind,da_cosk_5y,by=c("ym","SecCode"))

#names(da_m_ind)

da_m_ind[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),ivol=log(ivol))]
da_m_ind <- da_m_ind[order(ym,SecCode),]

#### The Regression Part 
lm_fit_nw <- function(nv) { # nw denotes Newey-West HAC t statistic
  # nv denotes numbers of variables
  lm_sta <- matrix(NA,nrow=nv+1,ncol=2) # sta denotes statistics
  for (i in 1:(nv+1)) {
    model_nw <- lm(lm_sta_raw[i,] ~ 1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(mean(as.numeric(unlist(summary(model)$adj.r.squared))))
}

####
da <- da_m_ind
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 3
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+size+BM|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 4
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+size+BM+mom|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 6
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+size+BM+mom+ret_tm+illiq|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 7
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+size+BM+mom+ret_tm+illiq+cosk_2|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ max_ret|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 2
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+max_ret|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 4
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+max_ret+size+BM|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 5
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+max_ret+size+BM+mom|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 7
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+max_ret+size+BM+mom+ret_tm+illiq|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 8
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=50,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e ~ be+max_ret+size+BM+mom+ret_tm+illiq+cosk_2|ind,da_sub)
  if (length(selected_industry)==1) {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][1])
    }
  } else {
    for (i in 1:(nv+1)) {
      lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
    }
  }
}
lm_fit_nw(nv)
now()


