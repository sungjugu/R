library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
# library(tidyverse)
library(ggplot2)
# library(ggpmisc)
library(plotly)
library(mgcv)
library(rcompanion)
library(car)
library(PerformanceAnalytics)
setwd("C:/r_data/농경연/쌀_콩_클라웃/")

################# 보간 ####################
dat_cli <- read.csv("dat_cli.csv", stringsAsFactors=F)

dat_cli <- dat_cli %>% mutate(year=as.integer(substr(date,1,4)), 
                              month=as.integer(substr(date,6,7)), 
                              day=as.integer(substr(date,9,10)),
                              half=ifelse(day>=15,1,2),
                              ten_day=ifelse(day<=10,1,ifelse(day>=21,3,2)))

# 필요 변수만 사용
dat_cli_redu <- dat_cli[,c(1:2,62:66,3:4,6,13,26,35,38)]
names(dat_cli_redu)

tt <- dat_cli %>% group_by(station) %>% tally()

#월단위 데이터 생성 
dat_cli_month <- dat_cli_redu %>% group_by(station,year,month) %>% summarise(tm_avg = mean(tm_avg, na.rm=T), 
                                                                             tm_min = mean(tm_min, na.rm=T), 
                                                                             tm_max = mean(tm_max, na.rm=T), 
                                                                             pr_sum = sum(pr_day, na.rm=T),
                                                                             hm_avg = mean(hm_avg, na.rm=T),
                                                                             ss_dura = mean(ss_dura_sum, na.rm=T),
                                                                             rad_avg = mean(rad_sum, na.rm=T))
dat_cli_month <- dat_cli_month[-c(1:5,45446),]
dat_cli_month <- dat_cli_month[!is.nan(dat_cli_month$rad_avg),]
dat_cli_month <- dat_cli_month[!is.nan(dat_cli_month$hm_avg),]
dat_cli_month <- dat_cli_month[!is.nan(dat_cli_month$ss_dura),]

tb_cli_miss <- dat_cli_month %>% group_by(station,year,month) %>% tally() %>% spread(station,n,fill=NA)
tb_cli_miss$station_num <- apply(tb_cli_miss[,3:ncol(tb_cli_miss)], 1, FUN=function(x) {sum(x,na.rm=T)})  # <------------------1973년부터 관측소 65개로 보간이 가능 

#10일단위 데이터 생성
dat_cli_tenday <- dat_cli_redu %>% group_by(station,year,month,ten_day) %>% summarise(tm_avg = mean(tm_avg, na.rm=T), 
                                                                                      tm_min = mean(tm_min, na.rm=T), 
                                                                                      tm_max = mean(tm_max, na.rm=T), 
                                                                                      pr_sum = sum(pr_day, na.rm=T),
                                                                                      hm_avg = mean(hm_avg, na.rm=T),
                                                                                      ss_dura = mean(ss_dura_sum, na.rm=T),
                                                                                      rad_avg = mean(rad_sum, na.rm=T)) %>% ungroup()
dat_cli_tenday <- dat_cli_tenday[-c(1:5,nrow(dat_cli_tenday)),]
dat_cli_tenday <- dat_cli_tenday[!is.nan(dat_cli_tenday$rad_avg),]
dat_cli_tenday <- dat_cli_tenday[!is.nan(dat_cli_tenday$hm_avg),]
dat_cli_tenday <- dat_cli_tenday[!is.nan(dat_cli_tenday$ss_dura),]

dat_cli_tenday <- as.data.frame(dat_cli_tenday)

tb_cli_miss <- dat_cli_tenday %>% group_by(station,year,month,ten_day) %>% tally() %>% spread(station,n,fill=NA)
tb_cli_miss$station_num <- apply(tb_cli_miss[,3:ncol(tb_cli_miss)], 1, FUN=function(x) {sum(x,na.rm=T)})  # <------------------1973년부터 관측소 65개로 보간이 가능 


#15일단위 데이터 생성
dat_cli_half <- dat_cli_redu %>% group_by(station,year,month,half) %>% summarise(tm_avg = mean(tm_avg, na.rm=T), 
                                                                                 tm_min = mean(tm_min, na.rm=T), 
                                                                                 tm_max = mean(tm_max, na.rm=T), 
                                                                                 pr_sum = sum(pr_day, na.rm=T),
                                                                                 hm_avg = mean(hm_avg, na.rm=T),
                                                                                 ss_dura = mean(ss_dura_sum, na.rm=T),
                                                                                 rad_avg = mean(rad_sum, na.rm=T)) %>% ungroup()

dat_cli_half <- dat_cli_half[-c(1:5,nrow(dat_cli_half)),]
dat_cli_half <- dat_cli_half[!is.nan(dat_cli_half$rad_avg),]
dat_cli_half <- dat_cli_half[!is.nan(dat_cli_half$hm_avg),]
dat_cli_half <- dat_cli_half[!is.nan(dat_cli_half$ss_dura),]


tb_cli_miss <- dat_cli_half %>% group_by(station,year,month,ten_day) %>% tally() %>% spread(station,n,fill=NA)
tb_cli_miss$station_num <- apply(tb_cli_miss[,3:ncol(tb_cli_miss)], 1, FUN=function(x) {sum(x,na.rm=T)})  # <------------------1973년부터 관측소 65개로 보간이 가능 



## Unknown or uninitialised column: 에러 제거 
class(dat_cli_half)
dat_cli_half <- as.data.frame(dat_cli_half)

# 시/군 기상 데이터 리스트 구성요소 정의 
region_list <- read.csv("kosis_region_2018.csv", stringsAsFactors=F)
year_list <- 2000:2021
month_list <- 1:12 #     지역 156개 * 46년 * 12개월 = 86112개, 지역은 552개 반복 후 전환, 아리) 지역 156 * 21년 * 12개월 


# 시/군 위경도 정보 불러오기
kor_adm <- read.csv("kor_adm_list.csv", stringsAsFactors=F)

# 기상관측소 위경도 정보 불러오기
wt_station <- read.csv("weather_station.csv", stringsAsFactors=F)
wt_station <- wt_station %>% mutate(year_start=as.integer(substr(date_start,1,4)))
wt_station <- wt_station[,-2]  

# 시/군 리스트에 위경도 및 가까운 기상관측소 3곳 및 거리 매칭
region_match <- merge(region_list, kor_adm[,1:4], by=c('wide','region'), all.x=T)
# region_match <- region_match %>% mutate(st1=NA, st2=NA, st3=NA, st4=NA, st5=NA, st6=NA, dist1=NA, dist2=NA, dist3=NA, dist4=NA, dist5=NA, dist6=NA)
region_match <- region_match %>% mutate(st1=NA, st2=NA, st3=NA, st4=NA, st5=NA, st6=NA, st7=NA, st8=NA, st9=NA, st10=NA, dist1=NA, dist2=NA, dist3=NA, dist4=NA, dist5=NA, dist6=NA, dist7=NA, dist8=NA, dist9=NA, dist10=NA)

for(i in 1:nrow(region_match)) { #i <- 1
  lat <- region_match$lat[i]
  long <- region_match$lng[i]
  tmp <- wt_station 
  tmp$dist <- sqrt((tmp$latitude - lat)^2+(tmp$longitude - long)^2)
  tmp <- tmp %>% arrange(dist) 
  region_match[i,5:14] <- t(tmp$station[1:10])
  region_match[i,15:24] <- t(tmp$dist[1:10])
}

# 시/군 과거 시점 기상 데이터 거리 보간 
dat_cli_adm <- data.frame(wide=rep(region_list[,2], each=252), 
                          region=rep(region_list[,1], each=252), 
                          year=rep(year_list, each=12, times=156),
                          month=rep(month_list, times=3276),
                          tm_avg=NA, tm_min=NA, tm_max=NA, pr_sum=NA, hm_avg=NA, ss_dura=NA)
# dat_cli_adm <- dat_cli_adm_na
for(i in 1:nrow(dat_cli_adm)) { i <- 1
year_i <- dat_cli_adm$year[i] # dat_cli_adm[868,]
month_i <- dat_cli_adm$month[i]
station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3))
dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3))
tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
if(nrow(tmp)!=3) {
  station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3, st4))
  dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3, dist4))
  tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
  if(nrow(tmp)!=3) {
    station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3, st4, st5))
    dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3, dist4, dist5))
    tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
    if(nrow(tmp)!=3) {
      station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3, st4, st5, st6))
      dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3, dist4, dist5, dist6))
      tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
      if(nrow(tmp)!=3) {
        station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3, st4, st5, st6, st7))
        dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3, dist4, dist5, dist6, dist7))
        tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
        if(nrow(tmp)!=3) {
          station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3, st4, st5, st6, st7, st8))
          dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8))
          tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
          if(nrow(tmp)!=3) {
            station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3, st4, st5, st6, st7, st8, st9))
            dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8, dist9))
            tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
            if(nrow(tmp)!=3) {
              station_i <- as.integer(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(st1, st2, st3, st4, st5, st6, st7, st8, st9, st10))
              dist_i <- as.numeric(region_match %>% filter(wide==dat_cli_adm$wide[i], region==dat_cli_adm$region[i]) %>% select(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8, dist9, dist10))
              tmp <- dat_cli_month %>% filter(year==year_i, month==month_i, station %in% station_i)
            }
          }
        }
      }
    }
  }
}
dist_i <- dist_i[which(station_i%in%as.integer(tmp$station))]
if(length(dist_i)==3){
  dat_cli_adm$tm_avg[i] <- (1/dist_i[1]*tmp$tm_avg[1] + 1/dist_i[2]*tmp$tm_avg[2] + 1/dist_i[3]*tmp$tm_avg[3]) / (1/dist_i[1]+1/dist_i[2]+1/dist_i[3])
  dat_cli_adm$tm_min[i] <- (1/dist_i[1]*tmp$tm_min[1] + 1/dist_i[2]*tmp$tm_min[2] + 1/dist_i[3]*tmp$tm_min[3]) / (1/dist_i[1]+1/dist_i[2]+1/dist_i[3])
  dat_cli_adm$tm_max[i] <- (1/dist_i[1]*tmp$tm_max[1] + 1/dist_i[2]*tmp$tm_max[2] + 1/dist_i[3]*tmp$tm_max[3]) / (1/dist_i[1]+1/dist_i[2]+1/dist_i[3])
  dat_cli_adm$pr_sum[i] <- (1/dist_i[1]*tmp$pr_sum[1] + 1/dist_i[2]*tmp$pr_sum[2] + 1/dist_i[3]*tmp$pr_sum[3]) / (1/dist_i[1]+1/dist_i[2]+1/dist_i[3])
  dat_cli_adm$hm_avg[i] <- (1/dist_i[1]*tmp$hm_avg[1] + 1/dist_i[2]*tmp$hm_avg[2] + 1/dist_i[3]*tmp$hm_avg[3]) / (1/dist_i[1]+1/dist_i[2]+1/dist_i[3])
  dat_cli_adm$ss_dura[i] <- (1/dist_i[1]*tmp$ss_dura[1] + 1/dist_i[2]*tmp$ss_dura[2] + 1/dist_i[3]*tmp$ss_dura[3]) / (1/dist_i[1]+1/dist_i[2]+1/dist_i[3])
  # dat_cli_adm$rad_avg[i] <- (1/dist_i[1]*tmp$rad_avg[1] + 1/dist_i[2]*tmp$rad_avg[2] + 1/dist_i[3]*tmp$rad_avg[3]) / (1/dist_i[1]+1/dist_i[2]+1/dist_i[3])  
} else if(length(dist_i)==2){
  dat_cli_adm$tm_avg[i] <- (1/dist_i[1]*tmp$tm_avg[1] + 1/dist_i[2]*tmp$tm_avg[2]) / (1/dist_i[1]+1/dist_i[2])
  dat_cli_adm$tm_min[i] <- (1/dist_i[1]*tmp$tm_min[1] + 1/dist_i[2]*tmp$tm_min[2]) / (1/dist_i[1]+1/dist_i[2])
  dat_cli_adm$tm_max[i] <- (1/dist_i[1]*tmp$tm_max[1] + 1/dist_i[2]*tmp$tm_max[2]) / (1/dist_i[1]+1/dist_i[2])
  dat_cli_adm$pr_sum[i] <- (1/dist_i[1]*tmp$pr_sum[1] + 1/dist_i[2]*tmp$pr_sum[2]) / (1/dist_i[1]+1/dist_i[2])
  dat_cli_adm$hm_avg[i] <- (1/dist_i[1]*tmp$hm_avg[1] + 1/dist_i[2]*tmp$hm_avg[2]) / (1/dist_i[1]+1/dist_i[2])
  dat_cli_adm$ss_dura[i] <- (1/dist_i[1]*tmp$ss_dura[1] + 1/dist_i[2]*tmp$ss_dura[2]) / (1/dist_i[1]+1/dist_i[2])
} else {
  dat_cli_adm$tm_avg[i] <- (1/dist_i[1]*tmp$tm_avg[1]) / (1/dist_i[1])
  dat_cli_adm$tm_min[i] <- (1/dist_i[1]*tmp$tm_min[1]) / (1/dist_i[1])
  dat_cli_adm$tm_max[i] <- (1/dist_i[1]*tmp$tm_max[1]) / (1/dist_i[1])
  dat_cli_adm$pr_sum[i] <- (1/dist_i[1]*tmp$pr_sum[1]) / (1/dist_i[1])
  dat_cli_adm$hm_avg[i] <- (1/dist_i[1]*tmp$hm_avg[1]) / (1/dist_i[1])
  dat_cli_adm$ss_dura[i] <- (1/dist_i[1]*tmp$ss_dura[1]) / (1/dist_i[1])
} 


}
names(dat_cli_month)
# dat_cli_adm_f3 <- dat_cli_adm
# dat_cli_adm_f2 <- dat_cli_adm3[!is.na(dat_cli_adm3$tm_avg),]
# dat_cli_adm_na <- dat_cli_adm3[is.na(dat_cli_adm3$tm_avg),]

# dat_cli_adm_m <- rbind(dat_cli_adm_f1,dat_cli_adm_f2,dat_cli_adm_f3)
# dat_cli_adm_m <- dat_cli_adm_m %>% arrange(wide,region,year,month)
# dat_cli_adm_na <- dat_cli_adm_m[is.na(dat_cli_adm_m$tm_avg),]
# dat_cli_adm <- dat_cli_adm_m

write.csv(dat_cli_adm,"D:/나의연구/학위논문/학위논문_new/분석/기상/dat_cli_adm_1973_2018.csv", row.names=F)
dat_cli_adm <- read.csv("D:/나의연구/학위논문/학위논문_new/분석/기상/dat_cli_adm_1973_2018.csv", stringsAsFactors=F)



### 기상청 데이터 가공 ###
# 1. 필요한 데이터 가져오기
dat_cli <- read.csv("dat_cli.csv", stringsAsFactors = F)
station_info <- read.csv("weather_station.csv", stringsAsFactors = F)

# 2. 기상청데이터 년, 월,일,보름,10일,pr_day 나누기
# pr_day => pr_day가 결측치면 0, 아니면 pr_day로 표시하기
dat_cli <- dat_cli %>% mutate(year=as.integer(substr(date,1,4)), 
                              month=as.integer(substr(date,6,7)), 
                              day=as.integer(substr(date,9,10)),
                              half=ifelse(day>=15,1,2),
                              ten_day=ifelse(day<=10,1,ifelse(day>=21,3,2)),
                              pr_day = ifelse(is.na(pr_day), 0, pr_day))

# 3. ws 변수 검토
names(dat_cli)
mean(dat_cli$ws_max, na.rm=T)
mean(dat_cli$ws_avg, na.rm=T)
max(dat_cli$ws_max, na.rm=T)
max(dat_cli$ws_avg, na.rm=T)
nrow(dat_cli[dat_cli$ws_avg>9,]) #ws_avg>9 인 행개수

# 4. 필요한 변수만 사용
dat_cli_redu <- dat_cli[,c(1:3,62:66,3:4,6,13,20,26,35,38)]
tt <- dat_cli %>% group_by(station) %>% tally()

# 5. station ingeger로 형변환
dat_cli_redu <- dat_cli_redu %>% mutate(station=as.integer(station)) %>% filter(!is.na(station))

# 6. 스테이션 정보와 기상정보 merge
dat_cli_sigun <- merge(dat_cli_redu, station_info[,c('station', 'wide','station_name','region')], by='station', all.x=T)


# 8. 평균값 구하기
# dat_cli_sigun <- dat_cli_sigun %>% group_by(date, region ,wide) %>% summarise(tm_avg = mean(tm_avg, na.rm=T),
#                                                                     tm_min = mean(tm_min, na.rm=T),
#                                                                     tm_max = mean(tm_max, na.rm=T),
#                                                                     pr_day = mean(pr_day, na.rm=T),
#                                                                     hm_avg = mean(hm_avg, na.rm=T),
#                                                                     ss_dura_sum = mean(ss_dura_sum, na.rm=T),
#                                                                     rad_sum = mean(rad_sum, na.rm=T),
#                                                                     ws_max = max(ws_avg, na.rm=T)
# )

# 9. date변수 존재하는 것만 남겨두기
dat_cli_sigun <- dat_cli_sigun %>% filter(str_length(date)>=10)

# 10. "강원", "경기", "경남", "경북", "전남", "전북", "충남", "충북" 만 남겨두기 (도단위)
dat_cli_sigun <- dat_cli_sigun %>% filter(wide%in%c("강원", "경기", "경남", "경북", "전남", "전북", "충남", "충북")) %>% select(-rad_sum)


# 11. 파일 저장
# write.csv(dat_cli_sigun, 'dat_cli_sigun_all(rice).csv', row.names = F)
dat_cli_sigun <- read.csv('dat_cli_sigun_all(rice).csv', stringsAsFactors = F)
dat_cli_sigun <- dat_cli_sigun %>% filter(wide%in%c("충남", "전북", "경남"))

# 12. 기상데이터 년,월,일,보름,10일 나누기
# month_tenday => 10이전 1, 10~19일 2, 20~31 3
# mon_third = month_tenday
# tenday_no => 몇주차 
dat_cli_sigun <- dat_cli_sigun %>% mutate(year=as.integer(substr(date,1,4)),
                                        month=as.integer(substr(date,6,7)), 
                                        day=as.integer(substr(date,9,10)),
                                        # half=ifelse(day>=15,1,2),
                                        ten_day=ifelse(day<=10,1,ifelse(day>=21,3,2)),
                                        mon_third=paste(month, ten_day, sep='_'),
                                        tenday_no=(month-1)*3+ten_day )

tt <- dat_cli_sigun %>% group_by(tenday_no, month, ten_day) %>% tally()

setwd("C:/r_data/농경연/쌀/")

### 배추 단수 데이터 ###
# 1. 데이터 불러오기
rice_prod_one <- read.csv("rice(2001~2010).csv", stringsAsFactors=F)
rice_prod_two <- read.csv("rice(2011~2021).csv", stringsAsFactors=F)

# 2. 컬럼명 변경 
colnames(rice_prod_one) <- c('wide','region','year','area_ha','prod_10a_kg','prod_ton')
colnames(rice_prod_two) <- c('wide','region','year','area_ha','prod_10a_kg','prod_ton')

# 3. 두 프레임 merge 
rice_prod <- rbind(rice_prod_one, rice_prod_two)

# 4. 정렬
rice_prod <- rice_prod %>% arrange(wide,region)

# 5. 트랜드 값 
fmla <- prod_10a_kg~year
fit.lm <- lm(fmla, rice_prod)
summary(fit.lm)
rice_prod <- rice_prod %>% mutate(prod_trend = -3727.9392 + 2.0988*year)

# 6. 단수 가공
dat_prod <- rice_prod %>% filter(!grepl('광역|특별',wide)) %>% mutate(wide = ifelse(wide%in%c('경기도','강원도','제주도'), substr(wide, 1,2),
                                                                                     paste(substr(wide,1,1), substr(wide,3,3), sep='')),
                                                                       prod_norm = prod_10a_kg - prod_trend)

# 7. 저장
# write.csv(dat_prod, 'rice(all).csv', row.names = F)
# dat_cli_prov <- read.csv('rice(all).csv', stringsAsFactors = F)

## 데이터셋 만들기 ##

## 쌀 생육단계 ##
# 이앙기 - 활착기 - 분얼기 - 유수형성기 - 수잉기 - 출수기 - 등숙기
# 4가지 경우 있음


# 충남, 전북, 경남 5/25 이앙
# P1 : 이앙기 5/25 ---------> 5월 3순기 (+0)
# P2 : 활착기, 분얼기 6/1 ~ 7/23 ---> 6월 1순기, 2순기, 3순기, 7월 1순기, 7월 2순기 (+1,2,3,4,5)
# P3 : 유수형성기, 수잉기 7/25 ~ 8/1 --> 7월 3순기 (+6)
# P4 : 출수기 8/19 ~ 8/27 -> 8월 2순기, 8월 3순기 (+7,8)
# P5 : 등숙기 8/28 ~ 9/22 -> 9월 1순기, 9월 2순기 (+9,10)

# 충남, 전북, 경남 5/30 이앙
# P1 : 이앙기 5/30 ----------> 5월 3순기 (+0)
# P2 : 활착기, 분얼기 6/6 ~7/27 ---> 6월 2순기, 3순기, 7월 1순기, 2순기, 3순기 (+1,2,3,4,5)
# P3 : 유수형성기,수잉기 7/28 ~ 8/11 -->8월 1순기 (+6)
# P4 : 출수기 8/22 ~ 8/29 ----------> 8월 2순기, 3순기 (+7,8)
# P5 : 등숙기 8/30 ~ 9/24 ----> 9월 1순기, 2순기 (+9,10)

# 충남, 전북, 경남 6/5 이앙
# P1 : 이앙기 6/5 -------> 6월 1순기 (+0)
# P2 : 활착기,분얼기 6/12 ~ 7/30 ----> 6월 2순기, 3순기, 7월 1순기, 2순기, 3순기 (+1,2,3,4,5)
# P3 : 유수형성기, 수잉기 7/31 ~ 8/13 --> 8월 1순기 (+6)
# P4 : 출수기 8/22 ~ 8/29 ---> 8월 2순기, 3순기 (+7,8)
# P5 : 등숙기 8/30 ~ 9/24 ---> 9월 1순기, 2순기 (+9,10)

# 충남, 전북, 경남 6/10 이앙
# P1 : 이앙기 6/10 -----> 6월 1순기 (+0)
# P2 : 활착기, 분얼기 6/17 ~ 8/1 ---> 6월 2순기, 3순기, 7월 1순기, 2순기, 3순기 (+1,2,3,4,5)
# P3 : 유수형성기, 수잉기 8/2 ~ 8/14 ---> 8월 1순기 (+6)
# P4 : 출수기 8/24 ~ 8/31 ---> 8월 2순기, 3순기 (+7,8)
# P5 : 등숙기 9/1 ~ 9/26 --> 9월 1순기, 2순기 (+9,10)

# --------------------------------------------------

# 경기, 충북, 전남, 경북 5/25 이앙
# P1 : 이앙기 5/25 --------> 5월 3순기 (+0) 
# P2 : 활착기, 분얼기 6/1 ~ 7/23  ---> 6월 1순기, 2순기, 3순기, 7월 1순기, 7월 2순기 (+1,2,3,4,5)
# P3 : 유수형성기, 수잉기 7/25 ~ 8/8 -->8월 1순기 (+6)
# P4 : 출수기 8/19 ~ 8/27 ----------> 8월 2순기, 3순기 (+7,8)
# P5 : 등숙기 8/28 ~ 9/22 ----> 9월 1순기, 2순기 (+9,10)

# 경기, 충북, 전남, 경북 5/30 이앙
# P1 : 이앙기 5/30 --------> 5월 3순기 (+0) 
# P2 : 활착기, 분얼기 6/6 ~ 7/27 ---> 6월 1순기, 2순기, 3순기, 7월 1순기, 7월 2순기 (+1,2,3,4,5)
# P3 : 유수형성기, 수잉기 7/28 ~ 8/11 --> 8월 1순기 (+6)
# P4 : 출수기 8/22 ~ 8/29 ---> 8월 2순기, 3순기 (+7,8)
# P5 : 등숙기 8/30 ~ 9/24 ---> 9월 1순기, 2순기 (+9,10)

# 경기, 충북, 전남, 경북 6/5 이앙
# P1 : 이앙기 6/5   -------> 6월 1순기 (+0) 
# P2 : 활착기,분얼기 6/12 ~ 7/30 ----> 6월 2순기, 3순기, 7월 1순기, 2순기, 3순기 (+1,2,3,4,5)
# P3 : 유수형성기,수잉기 7/31 ~ 8/13 --> 8월 1순기 (+6)
# P4 : 출수기 8/22 ~ 8/29 ---> 8월 2순기, 3순기 (+7,8)
# P5 : 등숙기 8/30 ~ 9/24 ---> 9월 1순기, 2순기 (+9,10)

# 경기, 충북, 전남, 경북 6/10 이앙
# P1 : 이앙기 6/10  -------> 6월 1순기 (+0) 
# P2 : 활착기,분얼기 6/17 ~ 8/1  ----> 6월 2순기, 3순기, 7월 1순기, 2순기, 3순기 (+1,2,3,4,5)
# P3 : 유수형성기,수잉기 8/2 ~ 8/14 --> 8월 1순기 (+6)
# P4 : 출수기 8/24 ~ 8/31 ---> 8월 2순기, 3순기 (+7,8)
# P5 : 등숙기 9/1 ~ 9/26 ---> 9월 1순기, 2순기 (+9,10)

# --------------------------------------------------

# 강원 5/10 이앙
# P1 : 이앙기 5/10 ----> 5월 1순기 (+0)
# P2 : 활착기,분얼기 5/17 ~ 7/7 ---> 5월 2순기, 3순기, 6월 1순기, 2순기, 3순기, 7월 1순기 (+1,2,3,4,5,6)
# P3 : 유수형성기, 수잉기 7/8 ~ 8/15 ---> 7월 2순기, 3순기, 8월 1순기(+7,8,9)
# P4 : 출수기 8/20 ~ 8/25 ---> 8월 2순기, 3순기 (+10,11,12)
# P5 : 등숙기 8/26 ~ 9/26 ---> 9월 1순기, 2순기 (+13, 14)

# 강원 5/20 이앙
# P1 : 이앙기 5/20 ----> 5월 2순기 (+0)
# P2 : 활착기, 분얼기 5/27 ~ 7/11 ---> 5월 3순기, 6월 1순기,2순기,3순기, 7월 1순기 (+1,2,3,4,5)
# P4 : 유수형성기, 수잉기 7/16 ~ 8/14  ----> 7월 2순기, 3순기, 8월 1순기 (+6,7,8)
# P6 : 출수기 8/23 ~ 8/29 ---> 8월 2순기,3순기 (+9,10)
# P7 : 등숙기 8/30 ~ 9/29 ---> 9월 1순기,2순기,3순기 (+11,12,13)

# 기준 
df <- dat_cli_sigun; year_from <- 2001; month <- 5; ten_day <- 3; var_name <- 'tm_avg'; stat_name <- 'mean'
names(df)
# names(df) <- c("date","wide","tm_avg","tm_min","tm_max","pr_day","hm_avg","ss_dura_sum","ws_max","year","month","day","ten_day","mon_third","tenday_no")
# df <- df %>% select("date","wide","region", "tm_avg","tm_min","tm_max","pr_day","hm_avg","ss_dura_sum","year","month","day","ten_day","mon_third","tenday_no")

# 함수 
df_wide_func <- function(df, year_from, month, ten_day, var_name, stat_name){
  tenday_no0 <- (month-1)*3+ten_day
  tmp_df <- df %>% filter(year>=year_from)
  
  if(month==5) { # 충남, 전북, 경남 5월 이앙
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no==(tenday_no0+0), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+1) & tenday_no<=(tenday_no0+5), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+6), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+7) & tenday_no<=(tenday_no0+8), 'P4',
                                                                     ifelse(tenday_no>=(tenday_no0+9) & tenday_no<=(tenday_no0+10), 'P5', NA
                                                                     )))))) %>% filter(!is.na(period))
  } else if(month==6) { # 충남, 전북, 경남 6월 이앙
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no==(tenday_no0+0), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+1) & tenday_no<=(tenday_no0+5), 'P2',
                                                       ifelse(tenday_no==(tenday_no0+6), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+7) & tenday_no<=(tenday_no0+8), 'P4',
                                                                     ifelse(tenday_no>=(tenday_no0+9) & tenday_no<=(tenday_no0+10), 'P5', NA
                                                                     )))))) %>% filter(!is.na(period))
  } else if(month==7) {
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+2), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+3) & tenday_no<=(tenday_no0+4), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+5) & tenday_no<=(tenday_no0+7), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+8) & tenday_no<=(tenday_no0+10), 'P4', NA
                                                              ))))) %>% filter(!is.na(period))
  }
  
  names(tmp_df)
  var_list <- c('wide', 'year', 'period', var_name)
  print(var_name)
  if(stat_name == 'mean'){
    df_wide <- tmp_df[,var_list] %>% group_by( wide, year, period) %>% summarise_each(funs(mean(., na.rm = TRUE)))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'sum'){
    df_wide <- tmp_df[,var_list] %>% group_by( wide, year, period) %>% summarise_each(funs(sum(., na.rm = TRUE)))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'max'){
    df_wide <- tmp_df[,var_list] %>% group_by( wide, year, period) %>% summarise_each(funs(max(., na.rm = TRUE)))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'min'){
    df_wide <- tmp_df[,var_list] %>% group_by( wide, year, period) %>% summarise_each(funs(min(., na.rm = TRUE)))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'gdd10c'){
    df_wide <- tmp_df[,var_list] %>% group_by( wide, year, period) %>% summarise_each(funs(gdd10c_func))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste('gdd10c', colnames(df_wide)[3:ncol(df_wide)], sep='_')
  }
  
  return(df_wide)
}

gdd10c_func <- function(x){
  y <- ifelse(sum(x-10, na.rm=T)>0, sum(x-10, na.rm=T), 0)
  return(y)
}

proc_list <- data.frame(var_names = c('tm_avg','tm_min','tm_max','hm_avg','pr_day','ss_dura_sum','ws_avg','tm_avg'),
                        stat_names = c('mean','mean','mean','mean','sum','mean','max','gdd10c'))

# dat_merge_5c 등등 나누기
year_sel <- 2001; month_sel <- 5; ten_day_sel <- 3
for(i in 1:nrow(proc_list)) { # i <- 8
  
  proc_list$var_names[i]
  proc_list$stat_names[i]
  
  if(i == 1) { 
    
    dat_merge <- df_wide_func(df, year_sel, month_sel, ten_day_sel, proc_list$var_names[i], proc_list$stat_names[i])  
  } else {
    tmp <- df_wide_func(df, year_sel, month_sel, ten_day_sel, proc_list$var_names[i], proc_list$stat_names[i])
    dat_merge <- cbind(dat_merge, tmp[,-c(1:2)])
  }
  
}

dat_merge <- merge(dat_merge, dat_prod[,c('wide','region', 'year', 'prod_10a_kg')], by=c('wide','region','year'), all.x=T)
dat_merge <- dat_merge[,c(1,2,ncol(dat_merge), 3:(ncol(dat_merge)-1))]
dat_merge <- dat_merge %>% arrange(wide,region)
names(dat_merge)
dat_merge <- dat_merge %>% select("wide", "region" , "year", "prod_10a_kg", "tm_avg_P1", "tm_avg_P2", "tm_avg_P3", "tm_avg_P4", "tm_avg_P5", "tm_min_P1", "tm_min_P2", "tm_min_P3", "tm_min_P4", "tm_min_P5", "tm_max_P1", "tm_max_P2", "tm_max_P3", "tm_max_P4", "tm_max_P5", "hm_avg_P1", "hm_avg_P2"
                     , "hm_avg_P3", "hm_avg_P4", "hm_avg_P5", "pr_day_P1" , "pr_day_P2", "pr_day_P3", "pr_day_P4", "pr_day_P5", "ss_dura_sum_P1", "ss_dura_sum_P2", "ss_dura_sum_P3", "ss_dura_sum_P4", "ss_dura_sum_P5", "ws_avg_P1", "ws_avg_P2", "ws_avg_P3", "ws_avg_P4", "ws_avg_P5", "gdd10c_P1", "gdd10c_P2"
                     , "gdd10c_P3", "gdd10c_P4", "gdd10c_P5" )
dat_merge <- dat_merge %>% filter(!grepl('소계',region))

dat_merge <- dat_merge[!(dat_merge$year == '2021'),]
write.csv(dat_merge, '강원_5월 이앙_2.csv', row.names = F)

## 분석 시작 ###
# 분석순서(지역) 회귀분석 -> 유의하지 않은 변수^2 회귀분석 -> 포물선에서 이상기후 찾기
# 분석순서(전국) 지역 변수 넣어서 회귀분석
# 데이터셋 순서) 충남,전북,경남_5월 -> 충남,전북,경남 6월 -> 경기,충북,전남,경북_5월 -> 경기,충북,전남,경북_6월 -> 강원도_5월_1 -> 강원도_5월_2

# 1. 데이터셋 불러오기 
충남_전북_경남_6월 <- read.csv("충남,전북,경남_6월 이앙.csv", stringsAsFactors=F)
dat_merge <- 충남_전북_경남_5월
dat_merge_충남 <- dat_merge %>% filter(wide=='충남')
# dat_merge_전국 <- read.csv('dat_merge_전국.csv', stringsAsFactors = F) %>% filter(wide=='강원')


# 2) 회귀분석
names(dat_merge)
fmla <- prod_10a_kg ~ tm_avg_P1 + tm_avg_P2 + tm_avg_P3 + tm_avg_P4 + tm_avg_P5 
fmla <- prod_10a_kg ~ tm_min_P1 + tm_min_P2 + tm_min_P3 + tm_min_P4 + tm_min_P5
fmla <- prod_10a_kg ~ tm_max_P1 + tm_max_P2 + tm_max_P3 + tm_max_P4 + tm_max_P5
fmla <- prod_10a_kg ~ hm_avg_P1 + hm_avg_P2 + hm_avg_P3 + hm_avg_P4 + hm_avg_P5
fmla <- prod_10a_kg ~ pr_day_P1 + pr_day_P2 + pr_day_P3 + pr_day_P4 + pr_day_P5
fmla <- prod_10a_kg ~ ss_dura_sum_P1 + ss_dura_sum_P2 + ss_dura_sum_P3 + ss_dura_sum_P4 + ss_dura_sum_P5
fmla <- prod_10a_kg ~ ws_max_P1 + ws_max_P2 + ws_max_P3 + ws_max_P4 + ws_max_P5
fmla <- prod_10a_kg ~ gdd10c_P1 + gdd10c_P2 + gdd10c_P3 + gdd10c_P4 + gdd10c_P5

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

# 3) 유의하지 않은 변수^2 회귀분석
fmla <- prod_10a_kg ~ tm_avg_P1 + tm_avg_P2 + tm_avg_P3 + I(tm_avg_P4^2) + tm_avg_P5 
fmla <- prod_10a_kg ~ tm_min_P1 + tm_min_P2 + I(tm_min_P3^2) + tm_min_P4 + tm_min_P5
fmla <- prod_10a_kg ~ tm_max_P1+ tm_max_P2 + I(tm_max_P3^2) + tm_max_P4 + I(tm_max_P5^2)
fmla <- prod_10a_kg ~ hm_avg_P1 + hm_avg_P2 + hm_avg_P3 + I(hm_avg_P4^2) + hm_avg_P5
fmla <- prod_10a_kg ~ I(pr_day_P1^2) + I(pr_day_P2^2) + pr_day_P3 + I(pr_day_P4^2) + pr_day_P5
fmla <- prod_10a_kg ~ ss_dura_sum_P1 + I(ss_dura_sum_P2^2) + ss_dura_sum_P3 + ss_dura_sum_P4 + ss_dura_sum_P5
fmla <- prod_10a_kg ~ ws_max_P1 + I(ws_max_P2^2) + ws_max_P3 + ws_max_P4 + ws_max_P5
fmla <- prod_10a_kg ~ gdd10c_P1 + gdd10c_P2 + I(gdd10c_P3^2) + gdd10c_P4 + gdd10c_P5

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

#4) 산포도, 추세선 그리기
ggplot(data=dat_merge_충남, aes(x=tm_avg_P1, y=prod_10a_kg, color=region, group=region)) + geom_point() +geom_smooth() 

gg <- ggplot(data=dat_merge_충남, aes(x=tm_avg_P1, y=prod_10a_kg, z=year, color=region, group=region)) + geom_point()
ggplotly(gg)


