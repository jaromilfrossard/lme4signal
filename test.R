rm(list=ls())
library(lme4)

lf= list.files("R")
for(i in lf){print(i);source(paste("R/",i,sep=""))}

devtools::install_github("jaromilfrossard/lme4signal")
library(lme4signal)

load("data_18i_20s.RData")




df=data_signal$design
signal20=data_signal$y[,1:20]

df$Anum = model.matrix(~A,df,contrasts.arg = list(A=contr.poly))[,-1]
df = cbind(df,as.matrix(model.matrix(~B,df,contrasts.arg = list(B=contr.poly))[,-1]))
df$Cnum = model.matrix(~C,df,contrasts.arg = list(C=contr.poly))[,-1]

colnames(df)[7:8] =c("Bnum1","Bnum2")
contrasts(df$A)= contr.poly
contrasts(df$B)= contr.poly
contrasts(df$C)= contr.poly




#flmer = y~A*B*C + Error(id/(B*C))+ Error(item/(A*C))

flmer = y~A*B*C + ((Bnum1+Bnum2)*Cnum||id)+ ((Anum*Cnum)||item)
f_signal = signal20~A*B*C + ((Bnum1+Bnum2)*Cnum||id)+ ((Anum*Cnum)||item)

m_signal = lmersignal(f_signal,df,REML = T)


anova(m_signal)

sapply(m_signal,function(o)o@optinfo$conv$opt)
lapply(m_signal,function(o)o@optinfo$warnings)
sapply(m_signal,function(o)o@optinfo$derivs$gradient)
sapply(m_signal,function(o){if(is.null(o@optinfo$conv$lme4$code)){0}else{o@optinfo$conv$lme4$code}})

lapply(m_signal,function(o)anova(o,type=3,ddf = "Satterthwaite"))

sapply(m_signal,function(o)o@optinfo$control$iprint)


thetas= getME(m_signal,"theta",SIMPLIFY=T)

ts.plot(t(thetas)[,c(1,7)])
rf=ranef(m_signal)
rfid = t(sapply(rf,function(rfi)rfi$id[,1]))

rfitem = t(sapply(rf,function(rfi)rfi$item[,1]))
ts.plot(rfitem)
ranef(m_signal)

