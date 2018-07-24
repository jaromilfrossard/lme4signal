rm(list=ls())
library(lme4)

# lf= list.files("R")
# for(i in lf){print(i);source(paste("R/",i,sep=""))}

devtools::install_github("jaromilfrossard/lme4signal")
library(lme4signal)

load("data_18i_20s.RData")




df=data_signal$design
signal20=data_signal$y[,1:10]

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


lx = getME(m_signal,"X",SIMPLIFY=T)

ranefmat= sapply(m_signal,function(model)ranef(model)$item[,1])
ts.plot(t(ranefmat))

ranefmat= sapply(m_signal,function(model)ranef(model)$id[,1])
ts.plot(t(ranefmat))


thetas = sapply(m_signal,function(model)getME(model,"theta"))
ts.plot(t( thetas))

mc$formula


lme4:::formula.

lme4:::getResponseFormula()
model.response(f_signal)
model.response(lmod$fr)

findbars(f_signal)
nobars(f_signal)
nlformula(f_signal)

df$y = data_signal$y[,1]
m_bobyqa= lmer(flmer,df,REML = T)

getME(m_bobyqa,"theta")

t0=proc.time()
m_bobyqa2 <- update(m_bobyqa, control = lmerControl(optimizer="bobyqa"))
proc.time()-t0

cbind(getME(m_bobyqa,"theta"),getME(m_bobyqa2,"theta"))



t0=proc.time()
m_nm <- update(m_bobyqa, control = lmerControl(optimizer="Nelder_Mead"))
proc.time()-t0
