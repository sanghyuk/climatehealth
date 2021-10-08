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

seoul_l_h<-subset(seoul_l,meantemp>=26)


library(mediation)
library(mgcv)


#temp. and ozone mediation in Seoul
med.fit<-gam(o3_ppb~meantemp+s(ddd,k=4*14)+meanhumi+totsun,family=gaussian(), data=seoul_l)
summary(med.fit)
plot(med.fit)

out.fit<-gam(all_tot~o3_ppb+meantemp+dow+s(ddd,k=4*14)+meanhumi+totsun,family=poisson(), data=seoul_l)
summary(out.fit)
plot(out.fit)

med.out<-mediate(med.fit, out.fit, treat="meantemp", mediator="o3_ppb", boot=T, sims=100)
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

#26도 이상
med.fit<-gam(o3_ppb~meantemp+meanhumi+rain+windspeed+s(ddd,k=4*14),family=gaussian(), data=seoul_l_h)
summary(med.fit)
plot(med.fit)

out.fit<-gam(all_tot~o3_ppb+meantemp+meanhumi+rain+windspeed+dow+s(ddd,k=4*14),family=poisson(), data=seoul_l_h)
summary(out.fit)
plot(out.fit)

med.out<-mediate(med.fit, out.fit, treat="meantemp", mediator="o3_ppb", boot=T, sims=10, control.value=0, treat.value = 1)
summary(med.out)
plot(med.out)

######ACME 0.0001273으로 일단 계산
###dataset
averyear<-read.csv(file="D:/Dropbox/기후보건영향평가/2021년/자료/일일건수_dataset_revsise_OJM/평년기온 6090.csv")
aveyear.seoul<-subset(averyear,siteno==108)
raw1.lag_seoul<-subset(raw1.lag,sido==11)
raw1.lag_seoul<-join(raw1.lag_seoul,aveyear.seoul,by=c("month", "day"))
summary(raw1.lag_seoul$ave_meantemp)
raw1.lag_seoul$avetemp_diff<-raw1.lag_seoul$meantemp-raw1.lag_seoul$ave_meantemp
summary(raw1.lag_seoul$avetemp_diff)
raw1.lag_seoul$avetemp_diff1<-ifelse(raw1.lag_seoul$avetemp_diff>0,raw1.lag_seoul$avetemp_diff,0)

raw1.lag_seoul$e_all_tot_avetemp<-raw1.lag_seoul$avetemp_diff1*raw1.lag_seoul$all_tot*((1.000127308-1)/1.000127308)
e_all_tot_avetemp<-ddply(raw1.lag_seoul,~area+year,summarise,BG=sum(e_all_tot_avetemp,na.rm=T))
write.csv(e_all_tot_avetemp,file="D:/Dropbox/기후보건영향평가/2021년/결과/avetemp_e_all_tot.csv")
