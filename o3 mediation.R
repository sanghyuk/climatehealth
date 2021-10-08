raw1<-read.csv(file="D:/Dropbox/기후보건영향평가/2021년/자료/raw.csv")
raw1$no2_ppb<-raw1$no2*1000
head(raw1$no2_ppb)
raw1$so2_ppb<-raw1$so2*1000
raw1$co_ppb<-raw1$co*1000
raw1$o3_ppb<-raw1$o3*1000
raw1$rain<-ifelse(is.na(raw1$rain),0,raw1$rain)
summary(raw1$rain)

seoul<-subset(raw1,sido==11)
busan<-subset(raw1,sido==26)
daegu<-subset(raw1,sido==27)
incheon<-subset(raw1,sido==28)
daejon<-subset(raw1,sido==29)
gwangju<-subset(raw1,sido==30)
ulsan<-subset(raw1,sido==31)
library(HEAT)
seoul_l<-lagdata(seoul,c("o3_ppb","meantemp","maxtemp","pm25"),28)
busan_l<-lagdata(busan,c("o3_ppb","meantemp","maxtemp","pm25"),28)
daegu_l<-lagdata(daegu,c("o3_ppb","meantemp","maxtemp","pm25"),28)
incheon_l<-lagdata(incheon,c("o3_ppb","meantemp","maxtemp","pm25"),28)
daejon_l<-lagdata(daejon,c("o3_ppb","meantemp","maxtemp","pm25"),28)
gwangju_l<-lagdata(gwangju,c("o3_ppb","meantemp","maxtemp","pm25"),28)
ulsan_l<-lagdata(ulsan,c("o3_ppb","meantemp","maxtemp","pm25"),28)

seoul_l$ddd<-1:6939
busan_l$ddd<-1:6939
daegu_l$ddd<-1:6939
incheon_l$ddd<-1:6939
daejon_l$ddd<-1:6939
ulsan_l$ddd<-1:6939
gwangju_l$ddd<-1:6939


#warm and cold season
seoul_l_w<-subset(seoul_l,month>3 & month<10)
seoul_l_c<-subset(seoul_l,month>9 | month<4)

library(mediation)
library(mgcv)


#temp. and ozone mediation in Seoul
med.fit<-gam(o3_ppb~s(meantemp)+meanhumi+rain+windspeed+s(ddd,k=4*14),family=gaussian(), data=seoul_l)
out.fit<-gam(all_tot~o3_ppb+s(meantemp)+meanhumi+rain+windspeed+dow+s(ddd,k=4*14),family=poisson(), data=seoul_l)
summary(med.fit)
plot(med.fit)
summary(out.fit)
plot(out.fit)
med.out<-mediate(med.fit, out.fit, treat="meantemp", mediator="o3_ppb", boot=T, sims=10)
summary(med.out)
plot(med.out)

##in warm season
med.fit.w<-gam(o3_ppb~meantemp+meanhumi+rain+windspeed+s(ddd,k=4*14),family=gaussian(), data=seoul_l_w)
out.fit.w<-gam(all_tot~o3_ppb+meantemp+meanhumi+rain+windspeed+dow+s(ddd,k=4*14),family=poisson(), data=seoul_l_w)
summary(med.fit.w)
plot(med.fit.w)
summary(out.fit.w)
plot(out.fit.w)
med.out.w<-mediate(med.fit.w, out.fit.w, treat="meantemp", mediator="o3_ppb", boot=T, sims=10)
summary(med.out.w)
plot(med.out.w)