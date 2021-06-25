raw<-read.csv(file="D:/Dropbox/기후보건영향평가/2021년/자료/raw.csv")
head(raw)
raw1<-subset(raw,year>2005)
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

#오존 총사망
raw1$bgo3diff<-raw1$o3_ppb-30
raw1$bgo3diff<-ifelse(raw1$bgo3diff>0,raw1$bgo3diff,0)
raw1$excessm_o3bg<-raw1$bgo3diff*raw1$nonacc_tot*0.000376629
raw1$excessm_o3bg_LL<-raw1$bgo3diff*raw1$nonacc_tot*0.000206335
raw1$excessm_o3bg_UL<-raw1$bgo3diff*raw1$nonacc_tot*0.000546894
library(plyr)
ex_bg<-ddply(raw1,~area+year,summarise,BG=sum(excessm_o3bg,na.rm=T),BG_LL=sum(excessm_o3bg_LL,na.rm=T),BG_UL=sum(excessm_o3bg_UL,na.rm=T))
ex_bg<-subset(ex_bg,year>2009)
write.csv(ex_bg,file="D:/Dropbox/기후보건영향평가/2021년/결과/o3exm.csv")
