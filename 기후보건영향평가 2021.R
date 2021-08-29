raw<-read.csv(file="D:/Dropbox/기후보건영향평가/2021년/자료/raw.csv")
head(raw)
raw1<-subset(raw,year>2009)
head(raw1)
raw1$no2_ppb<-raw1$no2*1000
head(raw1$no2_ppb)
raw1$so2_ppb<-raw1$so2*1000
raw1$co_ppb<-raw1$co*1000
raw1$o3_ppb<-raw1$o3*1000

#연도별 원인별 사망 건수 계산
library(dplyr)

allsum<-raw1 %>% 
  group_by(year) %>% 
  summarise(across(c(all_tot,nonacc_tot,circ_tot,ischHD_tot,MI_tot,cerebvas_tot,hemoStroke_tot,ischStroke_tot), sum))
write.csv(allsum, file="D:/Dropbox/기후보건영향평가/2021년/결과/allsum.csv")

#연도별 대기오염물질별 연평균 농도 계산
allpoll<-raw1 %>% 
  group_by(year) %>% 
  summarise(across(c(pm10,pm25,pm25_model,o3_ppb), mean,na.rm=T))
write.csv(allpoll, file="D:/Dropbox/기후보건영향평가/2021년/결과/allpoll.csv")

#CRF 용 데이터셋 (7개 대도시)
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

seoul_l$ddd<-1:3652
busan_l$ddd<-1:3652
daegu_l$ddd<-1:3652
incheon_l$ddd<-1:3652
daejon_l$ddd<-1:3652
ulsan_l$ddd<-1:3652
gwangju_l$ddd<-1:3652

raw2<-rbind(seoul_l,busan_l,daegu_l,incheon_l,daejon_l,gwangju_l,ulsan_l)
raw2$dow<-as.factor(raw2$dow)
raw2$f_sido<-as.factor(raw2$sido)
#오존 CRF
library(gamm4)
fit.o3.l0<-gamm4(nonacc_tot~o3_ppb+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l0$gam)
fit.o3.l1<-gamm4(nonacc_tot~o3_ppb_s1+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l1$gam)
fit.o3.l2<-gamm4(nonacc_tot~o3_ppb_s2+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l2$gam)
fit.o3.l3<-gamm4(nonacc_tot~o3_ppb_s3+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l3$gam)
fit.o3.l4<-gamm4(nonacc_tot~o3_ppb_s4+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l4$gam)
fit.o3.l5<-gamm4(nonacc_tot~o3_ppb_s5+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l5$gam)
fit.o3.l6<-gamm4(nonacc_tot~o3_ppb_s6+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l6$gam)
fit.o3.l7<-gamm4(nonacc_tot~o3_ppb_s7+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.l7$gam)

fit.o3.m1<-gamm4(nonacc_tot~o3_ppb_m1+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.m1$gam)
fit.o3.m2<-gamm4(nonacc_tot~o3_ppb_m2+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.m2$gam)
fit.o3.m3<-gamm4(nonacc_tot~o3_ppb_m3+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.m3$gam)
fit.o3.m4<-gamm4(nonacc_tot~o3_ppb_m4+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.m4$gam)
fit.o3.m5<-gamm4(nonacc_tot~o3_ppb_m5+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.m5$gam)
fit.o3.m6<-gamm4(nonacc_tot~o3_ppb_m6+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.m6$gam)
fit.o3.m7<-gamm4(nonacc_tot~o3_ppb_m7+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(fit.o3.m7$gam)

#오존 비사고 총사망 초과 사망
#초과 사망용 데이터셋 구성
raw1$area<-as.factor(raw1$area)
summary(raw1$area)
summary(raw1$sido)
raw1$sido_f<-as.factor(raw1$sido)
summary(raw1$sido_f)

raw.sub11<-subset(raw1,raw1$sido==11)
raw.sub26<-subset(raw1,raw1$sido==26)
raw.sub27<-subset(raw1,raw1$sido==27)
raw.sub28<-subset(raw1,raw1$sido==28)
raw.sub29<-subset(raw1,raw1$sido==29)
raw.sub30<-subset(raw1,raw1$sido==30)
raw.sub31<-subset(raw1,raw1$sido==31)
raw.sub41<-subset(raw1,raw1$sido==41)
raw.sub42<-subset(raw1,raw1$sido==42)
raw.sub43<-subset(raw1,raw1$sido==43)
raw.sub44<-subset(raw1,raw1$sido==44)
raw.sub45<-subset(raw1,raw1$sido==45)
raw.sub46<-subset(raw1,raw1$sido==46)
raw.sub47<-subset(raw1,raw1$sido==47)
raw.sub48<-subset(raw1,raw1$sido==48)
raw.sub49<-subset(raw1,raw1$sido==49)
raw.sub50<-subset(raw1,raw1$sido==50)

raw.sub11<-lagdata(raw.sub11,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub26<-lagdata(raw.sub26,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub27<-lagdata(raw.sub27,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub28<-lagdata(raw.sub28,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub29<-lagdata(raw.sub29,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub30<-lagdata(raw.sub30,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub31<-lagdata(raw.sub31,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub41<-lagdata(raw.sub41,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub42<-lagdata(raw.sub42,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub43<-lagdata(raw.sub43,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub44<-lagdata(raw.sub44,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub45<-lagdata(raw.sub45,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub46<-lagdata(raw.sub46,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub47<-lagdata(raw.sub47,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub48<-lagdata(raw.sub48,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub49<-lagdata(raw.sub49,c("o3_ppb","meantemp","maxtemp","pm25"),7)
raw.sub50<-lagdata(raw.sub50,c("o3_ppb","meantemp","maxtemp","pm25"),7)

raw1.lag<-rbind(raw.sub11,raw.sub26,raw.sub27,raw.sub28,raw.sub29,raw.sub30,raw.sub31,raw.sub41,raw.sub42,raw.sub43,raw.sub44,raw.sub45,raw.sub46,raw.sub47,raw.sub48,raw.sub49,raw.sub50)

raw1.lag$bgo3diff<-raw1.lag$o3_ppb_m5-30
raw1.lag$bgo3diff<-ifelse(raw1.lag$bgo3diff>0,raw1.lag$bgo3diff,0)
raw1.lag$excessm_o3bg<-raw1.lag$bgo3diff*raw1.lag$nonacc_tot*0.000750119
raw1.lag$excessm_o3bg_LL<-raw1.lag$bgo3diff*raw1.lag$nonacc_tot*0.000462761
raw1.lag$excessm_o3bg_UL<-raw1.lag$bgo3diff*raw1.lag$nonacc_tot*0.001037394
library(plyr)
ex_bg<-ddply(raw1.lag,~area+year,summarise,BG=sum(excessm_o3bg,na.rm=T),BG_LL=sum(excessm_o3bg_LL,na.rm=T),BG_UL=sum(excessm_o3bg_UL,na.rm=T))
ex_bg<-subset(ex_bg,year>2009)
write.csv(ex_bg,file="D:/Dropbox/기후보건영향평가/2021년/결과/o3exm.csv")

#PM2.5 단기CRF
#총사망
library(gamm4)
pm25.l0<-gamm4(all_tot~pm25+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l0$gam)
gc()
pm25.l1<-gamm4(all_tot~pm25_s1+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l1$gam)
pm25.l2<-gamm4(all_tot~pm25_s2+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l2$gam)
pm25.l3<-gamm4(all_tot~pm25_s3+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l3$gam)
pm25.l4<-gamm4(all_tot~pm25_s4+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l4$gam)
pm25.l5<-gamm4(all_tot~pm25_s5+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l5$gam)
pm25.l6<-gamm4(all_tot~pm25_s6+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l6$gam)
pm25.l7<-gamm4(all_tot~pm25_s7+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.l7$gam)

pm25.m1<-gamm4(all_tot~pm25_m1+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.m1$gam)
pm25.m2<-gamm4(all_tot~pm25_m2+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.m2$gam)
pm25.m3<-gamm4(all_tot~pm25_m3+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.m3$gam)
pm25.m4<-gamm4(all_tot~pm25_m4+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.m4$gam)
pm25.m5<-gamm4(all_tot~pm25_m5+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.m5$gam)
pm25.m6<-gamm4(all_tot~pm25_m6+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.m6$gam)
pm25.m7<-gamm4(all_tot~pm25_m7+s(meantemp)+s(meantemp_m7)+s(meantemp_m14)+meanhumi+dow+s(ddd,k=4*10),data=raw2,family=poisson(),random=~(1|f_sido))
summary(pm25.m7$gam)

#산출된 CRF 가 모두 음수이므로 당해년도에 산출한 CRF는 초과발생건수 추산에 적합하지 않음.

#PM2.5 초과총사망
raw1.lag$bgpm25diff<-raw1.lag$pm25_m1-2
raw1.lag$bgpm25diff<-ifelse(raw1.lag$bgpm25diff>0,raw1.lag$bgpm25diff,0)
raw1.lag$e_all_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$all_tot*((1.000337-1)/1.000337)
raw1.lag$e_all_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$all_tot*((1.000109-1)/1.000109)
raw1.lag$e_all_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$all_tot*((1.000564 -1)/1.000564 )
library(plyr)
e_all_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_all_tot_pm25,na.rm=T),BG_LL=sum(e_all_tot_pm25_LL,na.rm=T),BG_UL=sum(e_all_tot_pm25_UL,na.rm=T))
e_all_tot_pm25<-subset(e_all_tot_pm25,year>2009)
write.csv(e_all_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_all_tot.csv")

#PM2.5 초과 비사고총사망
raw1.lag$e_nonacc_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$nonacc_tot*((1.000395 -1)/1.000395 )
raw1.lag$e_nonacc_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$nonacc_tot*((1.000117 -1)/1.000117 )
raw1.lag$e_nonacc_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$nonacc_tot*((1.000672  -1)/1.000672  )
e_nonacc_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_nonacc_tot_pm25,na.rm=T),BG_LL=sum(e_nonacc_tot_pm25_LL,na.rm=T),BG_UL=sum(e_nonacc_tot_pm25_UL,na.rm=T))
write.csv(e_nonacc_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_nonacc_tot.csv")

#PM2.5 초과 심뇌혈관질환 사망
raw1.lag$e_circ_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$circ_tot*((1.000513  -1)/1.000513  )
raw1.lag$e_circ_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$circ_tot*((1.000170  -1)/1.000170  )
raw1.lag$e_circ_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$circ_tot*((1.000856   -1)/1.000856   )
e_circ_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_circ_tot_pm25,na.rm=T),BG_LL=sum(e_circ_tot_pm25_LL,na.rm=T),BG_UL=sum(e_circ_tot_pm25_UL,na.rm=T))
write.csv(e_circ_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_circ_tot.csv")

#PM2.5 초과 허혈성 심질환 사망
raw1.lag$e_ischHD_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$ischHD_tot*((1.000601  -1)/1.000601  )
raw1.lag$e_ischHD_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$ischHD_tot*((1.000093  -1)/1.000093  )
raw1.lag$e_ischHD_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$ischHD_tot*((1.001108   -1)/1.001108   )
e_ischHD_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_ischHD_tot_pm25,na.rm=T),BG_LL=sum(e_ischHD_tot_pm25_LL,na.rm=T),BG_UL=sum(e_ischHD_tot_pm25_UL,na.rm=T))
write.csv(e_ischHD_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_ischHD_tot.csv")

#PM2.5 초과 심근경색 사망
raw1.lag$e_MI_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$MI_tot*((1.000448-1)/1.000448)
raw1.lag$e_MI_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$MI_tot*((0.999918 -1)/0.999918 )
raw1.lag$e_MI_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$MI_tot*((1.000979 -1)/1.000979 )
e_MI_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_MI_tot_pm25,na.rm=T),BG_LL=sum(e_MI_tot_pm25_LL,na.rm=T),BG_UL=sum(e_MI_tot_pm25_UL,na.rm=T))
write.csv(e_MI_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_MI_tot.csv")

#PM2.5 뇌졸중 
raw1.lag$e_cerebvas_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$cerebvas_tot*((1.000699 -1)/1.000699 )
raw1.lag$e_cerebvas_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$cerebvas_tot*((1.000084 -1)/1.000084 )
raw1.lag$e_cerebvas_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$cerebvas_tot*((1.001315 -1)/1.001315 )
e_cerebvas_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_cerebvas_tot_pm25,na.rm=T),BG_LL=sum(e_cerebvas_tot_pm25_LL,na.rm=T),BG_UL=sum(e_cerebvas_tot_pm25_UL,na.rm=T))
write.csv(e_cerebvas_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_cerebvas_tot.csv")

#PM2.5 출혈성 뇌졸중
raw1.lag$e_hemoStroke_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$hemoStroke_tot*((1.000783 -1)/1.000783 )
raw1.lag$e_hemoStroke_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$hemoStroke_tot*((1.000170 -1)/1.000170 )
raw1.lag$e_hemoStroke_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$hemoStroke_tot*((1.001397 -1)/1.001397 )
e_hemoStroke_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_hemoStroke_tot_pm25,na.rm=T),BG_LL=sum(e_hemoStroke_tot_pm25_LL,na.rm=T),BG_UL=sum(e_hemoStroke_tot_pm25_UL,na.rm=T))
write.csv(e_hemoStroke_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_hemoStroke_tot.csv")

#PM2.5 허혈성 뇌졸중
raw1.lag$e_ischStroke_tot_pm25<-raw1.lag$bgpm25diff*raw1.lag$ischStroke_tot*((1.000698 -1)/1.000698 )
raw1.lag$e_ischStroke_tot_pm25_LL<-raw1.lag$bgpm25diff*raw1.lag$ischStroke_tot*((0.999900 -1)/0.999900 )
raw1.lag$e_ischStroke_tot_pm25_UL<-raw1.lag$bgpm25diff*raw1.lag$ischStroke_tot*((1.001496 -1)/1.001496 )
e_ischStroke_tot_pm25<-ddply(raw1.lag,~area+year,summarise,BG=sum(e_ischStroke_tot_pm25,na.rm=T),BG_LL=sum(e_ischStroke_tot_pm25_LL,na.rm=T),BG_UL=sum(e_ischStroke_tot_pm25_UL,na.rm=T))
write.csv(e_ischStroke_tot_pm25,file="D:/Dropbox/기후보건영향평가/2021년/결과/pm_e_ischStroke_tot.csv")
