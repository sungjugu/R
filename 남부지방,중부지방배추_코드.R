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

# 7. 스테이션 정보와 기상정보 merge
dat_cli_prov <- merge(dat_cli_redu, station_info[,c('station', 'wide')], by='station', all.x=T)

# 8. 평균값 구하기
dat_cli_prov <- dat_cli_prov %>% group_by(date, wide) %>% summarise(tm_avg = mean(tm_avg, na.rm=T),
                                                                    tm_min = mean(tm_min, na.rm=T),
                                                                    tm_max = mean(tm_max, na.rm=T),
                                                                    pr_day = mean(pr_day, na.rm=T),
                                                                    hm_avg = mean(hm_avg, na.rm=T),
                                                                    ss_dura_sum = mean(ss_dura_sum, na.rm=T),
                                                                    rad_sum = mean(rad_sum, na.rm=T),
                                                                    ws_max = max(ws_avg, na.rm=T)
)

# 9. date변수 존재하는 것만 남겨두기
dat_cli_prov <- dat_cli_prov %>% filter(str_length(date)>=10)

# 10. 강원", "경기", "경남", "경북", "전남", "전북", "충남", "충북" 만 남겨두기 (도단위)
dat_cli_prov <- dat_cli_prov %>% filter(wide%in%c("강원", "경기", "경남", "경북", "전남", "전북", "충남", "충북")) %>% select(-rad_sum)
# 고랭지배추 경우 
# dat_cli_prov <- dat_cli_prov %>% filter(station_name%in%"대관령") %>% select(-rad_sum)

# 11. 파일 저장
# write.csv(dat_cli_prov, 'dat_cli_prov_all(-jeju).csv', row.names = F)
dat_cli_prov <- read.csv('dat_cli_prov_all(-jeju).csv', stringsAsFactors = F)

# 12. 기상데이터 년,월,일,보름,10일 나누기
# month_tenday => 10이전 1, 10~19일 2, 20~31 3
# mon_third = month_tenday
# tenday_no => 몇주차 
dat_cli_prov <- dat_cli_prov %>% mutate(year=as.integer(substr(date,1,4)),
                                        month=as.integer(substr(date,6,7)), 
                                        day=as.integer(substr(date,9,10)),
                                        # half=ifelse(day>=15,1,2),
                                        ten_day=ifelse(day<=10,1,ifelse(day>=21,3,2)),
                                        mon_third=paste(month, ten_day, sep='_'),
                                        tenday_no=(month-1)*3+ten_day )

tt <- dat_cli_prov %>% group_by(tenday_no, month, ten_day) %>% tally()


setwd("C:/r_data/농경연/배추/")

### 배추 단수 데이터 ###
# 1. 데이터 불러오기
cabbage_prod <- read.csv("노지봄배추(2001~2020).csv", stringsAsFactors=F)

# 2. 컬럼명 변경 
colnames(cabbage_prod) <- c('region','year','area_ha','prod_10a_kg','prod_ton')

# 3. 트랜드 값 
# fmla <- prod_10a_kg~year
# fit.lm <- lm(fmla, cabbage_prod)
# summary(fit.lm)

# 4. 강원도만 남겨두기
# 경기, 강원, 제주 나머지는 충북, 충남, 경북, 경남 등 처리하기 
dat_prod <- cabbage_prod %>% filter(!grepl('광역|특별|계',region)) %>% mutate(wide = ifelse(region%in%c('경기도','강원도','제주도'), substr(region, 1,2),
                                                                                       paste(substr(region,1,1), substr(region,3,3), sep='')))

prod_wide <- dat_prod %>% select(year, wide, prod_10a_kg) %>% spread(wide, prod_10a_kg, fill=NA)
prod_wide$전국평균 <- round(apply(prod_wide[,2:ncol(prod_wide)], 1, mean),1)
cor_prod <- round(cor(prod_wide[,c(2:ncol(prod_wide))]),3)



### 데이터셋 만들기 ###

## 배추 생육단계 ##
# 파종 5/20 ~ 6/10
# 정식 6/11 ~ 6/30
# 결구기 7월 11일 ~ 8월 10일 
# 수확기 8월 11일 ~ 8월 20일 

## 파종일 설정 ##
# 5/20, 5/25, 5/30, 6/5, 6/10 

## 단수 영향 요인 ##
# 가뭄, 기온상승, 장마 

## 시기별 단수 감소 영향 요인 ##
# 파종     : 가뭄(-), 장마(-)
# 정식     : 가뭄(-), 장마(-), 기온상승(-), 토양수분(-)
# 결구기   : 가뭄(-), 장마(-), 기온상승(-), 고온(-)
# 수확기   : 장마(-)( 2010년의 경우 잦은 강우로 인한 정식 지체로 생육 완료가 지연되어 수확이 10~15일 지연됨)

# * 남부지방 가을배추(경남,경북,전남,전북)
# (P1) 파종 8/11 ~ 8/31  ----> 8월 2순기, 8월 3순기 (+0,1)            
# (P2) 정식 9/1 ~ 9/10 ----> 9월 1순기 (+2)          
# (P3) 결구기 9월 11일 ~ 10월 31일  --> 9월 2순기, 9월 3순기, 10월 1순기, 10월 2순기, 10월 3순기 (+3,4,5,6,7)  
# (P4) 수확기 11월 1일 ~ 12월 10일 ---> 11월 1순기, 11월 2순기, 11월 3순기, 12월 1순기 (+8,9,10,11)

# * 중부지방 가을배추(경기,충북,충남)
# (P1) 파종 8/1 ~ 8/20  ----> 8월 1순기, 8월 2순기 (+0,1)            
# (P2) 정식 8/21 ~ 8/30 ----> 8월 3순기 (+2)          
# (P3) 결구기 9월 1일 ~ 10월 31일  --> 9월 1순기 ,9월 2순기, 9월 3순기, 10월 1순기, 10월 2순기, 10월 3순기 (+3,4,5,6,7,8)  
# (P4) 수확기 11월 1일 ~ 12월 10일 ---> 11월 1순기, 11월 2순기, 11월 3순기, 12월 1순기 (+9,10,11,12)

# * 남부지방 봄배추(전남,경남)
# https://www.busan.go.kr/nongup/agrihealthveget07
# (P1) 파종 4/1 ~ 5/10 ---> 4월 1순기, 4월 2순기, 4월 3순기, 5월 1순기 (+0,1,2,3)
# (P2) 정식 5/11 ~ 5/20 --> 5월 2순기 (+4)
# (P3) 결구기 5/21 ~ 6/10 -> 5월 3순기, 6월 1순기 (+5,6)
# (P4) 수확 6/11 ~ 7/20 ---> 6월 2순기, 6월 3순기, 7월 1순기, 7월 2순기 (+7,8,9,10)

# * 중부지방 봄배추(경기,충남)
# http://www.nongsaro.go.kr/portal/ps/psb/psbk/kidoContentsFileView.mo?kidofcomdtyNo=22565
# https://blog.daum.net/yi2355/12367554
# (P1) 파종 3/1 ~ 4/10 ----> 3월 1순기, 3월 2순기, 3월 3순기, 4월 1순기 (+0,1,2,3)
# (P2) 정식 4/11 ~ 5/30 ---> 4월 2순기, 4월 3순기, 5월 1순기, 5월 2순기, 5월 3순기 (+4,5,6,7,8)
# (P3) 결구기 6/1 ~ 6/ 20 -> 6월 1순기, 6월 2순기 (+9,10)
# (P4) 수확기 6/21 ~ 7/10 -> 6월 3순기, 7월 1순기 (+11,12)


# 기준 
df <- dat_cli_prov; year_from <- 2001; month <- 4; ten_day <- 1; var_name <- 'tm_avg'; stat_name <- 'mean'
names(df)
# names(df) <- c("date","wide","tm_avg","tm_min","tm_max","pr_day","hm_avg","ss_dura_sum","ws_max","year","month","day","ten_day","mon_third","tenday_no")

# 함수 
df_wide_func <- function(df, year_from, month, ten_day, var_name, stat_name){
  tenday_no0 <- (month-1)*3+ten_day
  tmp_df <- df %>% filter(year>=year_from)
  
  if(month==4) { #남부지방(봄)
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+3), 'P1',
                                                ifelse(tenday_no==(tenday_no0+4), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+5) & tenday_no<=(tenday_no0+6), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+7) & tenday_no<=(tenday_no0+10), 'P4', NA
                                                              ))))) %>% filter(!is.na(period))
  } else if(month==5) {
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+1), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+2) & tenday_no<=(tenday_no0+4), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+5) & tenday_no<=(tenday_no0+7), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+8), 'P4', NA
                                                              ))))) %>% filter(!is.na(period))
  } else if(month==7) {
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+2), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+3) & tenday_no<=(tenday_no0+4), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+5) & tenday_no<=(tenday_no0+7), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+8) & tenday_no<=(tenday_no0+10), 'P4', NA
                                                              ))))) %>% filter(!is.na(period))
  } else if(month>=8) { #남부지방(가을)
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+1), 'P1',
                                                ifelse(tenday_no==(tenday_no0+2), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+3) & tenday_no<=(tenday_no0+7), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+8) & tenday_no<=(tenday_no0+11), 'P4', NA
                                                              ))))) %>% filter(!is.na(period))
  } else if(month==3) { #중부지방(봄)
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+3), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+4) & tenday_no<=(tenday_no0+8), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+9) & tenday_no<=(tenday_no0+10), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+11) & tenday_no<=(tenday_no0+12), 'P4', NA
                                                              ))))) %>% filter(!is.na(period))
  }
  # else if(month>=8) { #중부지방(가을)
  #   tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+1), 'P1',
  #                                               ifelse(tenday_no==(tenday_no0+2), 'P2',
  #                                                      ifelse(tenday_no>=(tenday_no0+3) & tenday_no<=(tenday_no0+8), 'P3',
  #                                                             ifelse(tenday_no>=(tenday_no0+9) & tenday_no<=(tenday_no0+12), 'P4', NA
  #                                                             ))))) %>% filter(!is.na(period))
  # }
  
  names(tmp_df)
  var_list <- c('wide', 'year', 'period', var_name)
  print(var_name)
  if(stat_name == 'mean'){
    df_wide <- tmp_df[,var_list] %>% group_by(wide, year, period) %>% summarise_all(funs(mean))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'sum'){
    df_wide <- tmp_df[,var_list] %>% group_by(wide, year, period) %>% summarise_all(funs(sum))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'max'){
    df_wide <- tmp_df[,var_list] %>% group_by(wide, year, period) %>% summarise_all(funs(max))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'min'){
    df_wide <- tmp_df[,var_list] %>% group_by(wide, year, period) %>% summarise_all(funs(min))
    colnames(df_wide)[4] <- 'var_sel'
    df_wide <- df_wide %>% spread(period, var_sel, fill=NA)  
    colnames(df_wide)[3:ncol(df_wide)] <- paste(var_name, colnames(df_wide)[3:ncol(df_wide)], sep='_')
  } else if(stat_name == 'gdd10c'){
    df_wide <- tmp_df[,var_list] %>% group_by(wide, year, period) %>% summarise_all(funs(gdd10c_func))
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

proc_list <- data.frame(var_names = c('tm_avg','tm_min','tm_max','hm_avg','pr_day','ss_dura_sum','ws_max','tm_avg'),
                        stat_names = c('mean','mean','mean','mean','sum','mean','max','gdd10c'))

# dat_merge_5c 등등 나누기
year_sel <- 2001; month_sel <- 4; ten_day_sel <- 1
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

dat_merge <- merge(dat_merge, dat_prod[,c('wide', 'year', 'prod_10a_kg')], by=c('wide', 'year'), all.x=T)
dat_merge <- dat_merge[,c(1,2,ncol(dat_merge), 3:(ncol(dat_merge)-1))]

dat_merge_4a_남부지방_봄 <- dat_merge %>% filter(wide%in%c('전남','경남'))
dat_merge_4a_남부지방_봄 <- dat_merge_4a_남부지방_봄[!(dat_merge_4a_남부지방_봄$year == '2021'),]
write.csv(dat_merge_4a_남부지방_봄, 'dat_merge_4a_남부지방_봄.csv', row.names = F)
dat_merge <- read.csv('dat_merge_8a_중부지방.csv', stringsAsFactors = F)



## 분석 시작 ###
# 분석순서) 회귀분석 -> 유의하지 않은 변수^2 회귀분석 -> plot 확인(포물선 확인) -> 포물선 그리는 경우 산포도+추세선 그려서 확인하기
# 데이터셋 순서) dat_merge_4a_대관령 -> dat_merge_5c_대관령 -> dat_merge_7a_대관령 -> dat_merge_8a_중부지방 -> dat_merge_8b_남부지방

# 1. 데이터셋 불러오기 
dat_merge_4a_강원도 <- read.csv("dat_merge_4a_강원도.csv", stringsAsFactors=F)
dat_merge_5c_강원도 <- read.csv("dat_merge_5c_강원도.csv", stringsAsFactors=F)
dat_merge_7a_강원도 <- read.csv("dat_merge_7a_강원도.csv", stringsAsFactors=F)
dat_merge_8a_중부지방_가을 <- read.csv("dat_merge_8a_중부지방_가을.csv", stringsAsFactors=F)
dat_merge_8b_남부지방_가을 <- read.csv("dat_merge_8b_남부지방_가을.csv", stringsAsFactors=F)
dat_merge_3a_중부지방_봄 <- read.csv("dat_merge_3a_중부지방_봄.csv", stringsAsFactors=F)
dat_merge_4a_남부지방_봄 <- read.csv("dat_merge_4a_남부지방_봄.csv", stringsAsFactors=F)
dat_merge_전국 <- read.csv("dat_merge_전국.csv", stringsAsFactors=F)

dat_merge_전국 <- read.csv('dat_merge_전국.csv', stringsAsFactors = F) %>% filter(wide=='강원')
dat_merge <- dat_merge_4a_강원도

# 2) 회귀분석
names(dat_merge)
fmla <- prod_10a_kg ~ tm_avg_P1 + tm_avg_P2 + tm_avg_P3 + tm_avg_P4 
fmla <- prod_10a_kg ~ tm_min_P1 + tm_min_P2 + tm_min_P3 + tm_min_P4 
fmla <- prod_10a_kg ~ tm_max_P1 + tm_max_P2 + tm_max_P3 + tm_max_P4 
fmla <- prod_10a_kg ~ hm_avg_P1 + hm_avg_P2 + hm_avg_P3 + hm_avg_P4 
fmla <- prod_10a_kg ~ pr_day_P1 + pr_day_P2 + pr_day_P3 + pr_day_P4
fmla <- prod_10a_kg ~ ss_dura_sum_P1 + ss_dura_sum_P2 + ss_dura_sum_P3 + ss_dura_sum_P4 
fmla <- prod_10a_kg ~ ws_max_P1 + ws_max_P2 + ws_max_P3 + ws_max_P4 
fmla <- prod_10a_kg ~ gdd10c_P1 + gdd10c_P2 + gdd10c_P3 + gdd10c_P4 

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

# 3) 유의하지 않은 변수^2 회귀분석
fmla <- prod_10a_kg ~ I(tm_avg_P1^2) + I(tm_avg_P2^2) + I(tm_avg_P3^2) + tm_avg_P4 
fmla <- prod_10a_kg ~ I(tm_min_P1^2) + tm_min_P2 + tm_min_P3 + I(tm_min_P4^2)
fmla <- prod_10a_kg ~ I(tm_max_P1^2) + I(tm_max_P2^2) + tm_max_P3 + I(tm_max_P4^2)
fmla <- prod_10a_kg ~ hm_avg_P1 + hm_avg_P2 + I(hm_avg_P3^2) + hm_avg_P4
fmla <- prod_10a_kg ~ I(pr_day_P1^2) + I(pr_day_P2^2) + pr_day_P3 + I(pr_day_P4^2)
fmla <- prod_10a_kg ~ ss_dura_sum_P1 + I(ss_dura_sum_P2^2) + I(ss_dura_sum_P3^2) + ss_dura_sum_P4
fmla <- prod_10a_kg ~ I(ws_max_P1^2) + I(ws_max_P2^2) + I(ws_max_P3^2) + ws_max_P4
fmla <- prod_10a_kg ~ I(gdd10c_P1^2) + I(gdd10c_P2^2) + gdd10c_P3 + gdd10c_P4

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

# 4) plot 확인(포물선 확인)
vif(fit.lm)
chart.Correlation(dat_merge[,c(4:7,3)], histogram = T, pch='+') # 평균온도 
chart.Correlation(dat_merge[,c(8:11,3)], histogram = T, pch='+') # 최저온도 
chart.Correlation(dat_merge[,c(12:15,3)], histogram = T, pch='+') # 최고온도 
chart.Correlation(dat_merge[,c(16:19,3)], histogram = T, pch='+') # 평균습도 
chart.Correlation(dat_merge[,c(20:23,3)], histogram = T, pch='+') # 강수량
chart.Correlation(dat_merge[,c(24:27,3)], histogram = T, pch='+') # 일조시간 
chart.Correlation(dat_merge[,c(28:31,3)], histogram = T, pch='+') # 최대풍속
chart.Correlation(dat_merge[,c(32:35,3)], histogram = T, pch='+') # GDD

# 5) 포물선 그리는 경우 꼭지점 확인하기
ggplot(data=dat_merge, aes(x=gdd10c_P4, y=prod_10a_kg)) + geom_point() +geom_smooth() 

# 6) 포물선 꼭지점 계산
fmla <- prod_10a_kg ~ tm_avg_P1 + I((tm_avg_P2-23)^2) + tm_avg_P3 + tm_avg_P4 
fmla <- prod_10a_kg ~ tm_min_P1 + I((tm_min_P2-18.5)^2) + I((tm_min_P3-12.1)^2) + tm_min_P4
fmla <- prod_10a_kg ~ I((tm_max_P1-28)^2) + I((tm_max_P2-28.9)^2) + tm_max_P3 + tm_max_P4
fmla <- prod_10a_kg ~ hm_avg_P1 + hm_avg_P2 + I((hm_avg_P3-72)^2) + hm_avg_P4 
fmla <- prod_10a_kg ~ I((pr_day_P1-421)^2) + I((pr_day_P2-300)^2) + pr_day_P3 + I((pr_day_P4-50)^2)
fmla <- prod_10a_kg ~ I((ss_dura_sum_P1-6.4)^2) + ss_dura_sum_P2 + ss_dura_sum_P3 + ss_dura_sum_P4 
fmla <- prod_10a_kg ~ I((ws_max_P1-10)^2) + ws_max_P2 + I((ws_max_P3-10.5)^2) + ws_max_P4
fmla <- prod_10a_kg ~ gdd10c_P1 + I((gdd10c_P2-130)^2) + I((gdd10c_P3-380)^2) + gdd10c_P4 

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

#6) 산포도, 추세선 그리기
gg <- ggplot(data=dat_merge, aes(x=gdd10c_P4, y=prod_10a_kg, z=year, color=wide, group=wide)) + geom_point()
ggplotly(gg)


# 7) 전체 데이터프레임(전국)
dat_merge_4a_남부지방_봄 <- subset(dat_merge_4a_남부지방_봄, select = -code)
dat_merge_4a_남부지방_봄 <- cbind(dat_merge_4a_남부지방_봄, code=rep("남부_봄", nrow(dat_merge_4a_남부지방_봄)))
dat_merge <- rbind(dat_merge_4a_강원도,dat_merge_5c_강원도,dat_merge_7a_강원도,dat_merge_8a_중부지방_가을,dat_merge_8b_남부지방_가을,dat_merge_3a_중부지방_봄,dat_merge_4a_남부지방_봄)
dat_merge <- dat_merge %>% mutate(D_GW = ifelse(wide=='강원',1,0), D_GG = ifelse(wide=='경기',1,0), D_CB = ifelse(wide=='충북',1,0), D_CN = ifelse(wide=='충남',1,0), D_GB = ifelse(wide=='경북',1,0), D_JB = ifelse(wide=='전북',1,0), D_JN = ifelse(wide=='전남',1,0))
# write.csv(dat_merge, 'dat_merge_전국.csv', row.names = F)

# 8) 회귀분석
fmla <- prod_10a_kg ~ D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN

# 유의 변수 D_GW,D_GB,D_JB
fmla <- prod_10a_kg ~ tm_avg_P2 + tm_avg_P3 + tm_avg_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ tm_min_P3 + tm_min_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN 
fmla <- prod_10a_kg ~ tm_max_P1 + tm_max_P2 + tm_max_P3 + tm_max_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN 
fmla <- prod_10a_kg ~ hm_avg_P1 + hm_avg_P2 + hm_avg_P3 + hm_avg_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN 
fmla <- prod_10a_kg ~ pr_day_P1 + pr_day_P2 + pr_day_P3 + pr_day_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ ss_dura_sum_P1 + ss_dura_sum_P2 + ss_dura_sum_P3 + ss_dura_sum_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ ws_max_P1 + ws_max_P2 + ws_max_P3 + ws_max_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ gdd10c_P1 + gdd10c_P2 + gdd10c_P3 + gdd10c_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

# 9) 유의하지 않은 변수 회귀분석
fmla <- prod_10a_kg ~ tm_avg_P1 + tm_avg_P2 + tm_avg_P3 + I(tm_avg_P4^2) + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ tm_min_P1 + tm_min_P2 + I(tm_min_P3^2) + I(tm_min_P4^2) + D_GW + D_GG + D_CB + D_GB + D_JN
fmla <- prod_10a_kg ~ I(tm_max_P1^2) + I(tm_max_P2^2) + I(tm_max_P3^2) + I(tm_max_P4^2) + D_GW + D_GG + D_CB + D_GB + D_JN
fmla <- prod_10a_kg ~ hm_avg_P1 + I(hm_avg_P2^2) + I(hm_avg_P3^2) + hm_avg_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ pr_day_P1 + I(pr_day_P2^2) + pr_day_P3 + pr_day_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ ss_dura_sum_P1 + ss_dura_sum_P2 + I(ss_dura_sum_P3^2) + ss_dura_sum_P4 + D_GW + D_GG + D_CB + D_CN + D_GB + D_JB + D_JN
fmla <- prod_10a_kg ~ I(ws_max_P1^2) + I(ws_max_P2^2) + ws_max_P3 + I(ws_max_P4^2) + D_GW + D_GG + D_CB + D_GB + D_JN
fmla <- prod_10a_kg ~ I(gdd10c_P1^2) + I(gdd10c_P2^2) + I(gdd10c_P3^2) + I(gdd10c_P4^2) + D_GW + D_GG + D_CB + D_GB + D_JN

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

# 10) plot 확인(포물선 확인) (위 코드로 진행)

# 11) 포물선 그리는 경우 꼭지점 확인하기 (위 코드로 진행)

# 12) 포물선 꼭지점 계산
fmla <- prod_10a_kg ~ tm_avg_P1 + tm_avg_P2 + I((tm_avg_P3-18.5)^2) + tm_avg_P4 + D_GW + D_GG + D_CB + D_GB + D_JN
fmla <- prod_10a_kg ~ tm_min_P1 + I((tm_min_P2-19)^2) + I((tm_min_P3-14)^2) + tm_min_P4 + D_GW + D_GG + D_CB + D_GB + D_JN 
fmla <- prod_10a_kg ~ I((tm_max_P1-29)^2) + I((tm_max_P2-19)^2) + I((tm_max_P3-24)^2) + tm_max_P4 + D_GW + D_GG + D_CB + D_GB + D_JN 
fmla <- prod_10a_kg ~ I((hm_avg_P1-78)^2) + I((hm_avg_P2-75)^2) + hm_avg_P3 + hm_avg_P4 + D_GW + D_GG + D_CB + D_GB + D_JN 
fmla <- prod_10a_kg ~ I((pr_day_P1-250)^2) + pr_day_P2 + pr_day_P3 + pr_day_P4 + D_GW + D_GG + D_CB + D_GB + D_JN
fmla <- prod_10a_kg ~ ss_dura_sum_P1 + ss_dura_sum_P2 + I((ss_dura_sum_P3-6.9)^2) + I((ss_dura_sum_P4-6)^2) + D_GW + D_GG + D_CB + D_GB + D_JN 
fmla <- prod_10a_kg ~ ws_max_P1 + ws_max_P2 + ws_max_P3 + ws_max_P4 + D_GW + D_GG + D_CB + D_GB + D_JN
fmla <- prod_10a_kg ~ gdd10c_P1 + I((gdd10c_P2-125)^2) + gdd10c_P3 + gdd10c_P4 + D_GW + D_GG + D_CB + D_GB + D_JN

fit.lm <- lm(fmla, dat_merge)
summary(fit.lm)
vif(fit.lm)

# 13) 산포도, 추세선 그리기 (위 코드로 진행)
