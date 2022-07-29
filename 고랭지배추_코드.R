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

# 7. 평균값 구하기
dat_cli_prov <- dat_cli_sigun %>% group_by(date, station_name) %>% summarise(tm_avg = mean(tm_avg, na.rm=T),
                                                                    tm_min = mean(tm_min, na.rm=T),
                                                                    tm_max = mean(tm_max, na.rm=T),
                                                                    pr_day = mean(pr_day, na.rm=T),
                                                                    hm_avg = mean(hm_avg, na.rm=T),
                                                                    ss_dura_sum = mean(ss_dura_sum, na.rm=T),
                                                                    rad_sum = mean(rad_sum, na.rm=T),
                                                                    ws_max = max(ws_avg, na.rm=T))
                                                                    

# 8. date변수 존재하는 것만 남겨두기
dat_cli_prov <- dat_cli_prov %>% filter(str_length(date)>=10)

# 9. 대관령만 남겨두기
dat_cli_prov <- dat_cli_prov %>% filter(station_name%in%"대관령") %>% select(-rad_sum)

# 10. 대관령->강원도로 바꾸기
dat_cli_prov$station_name <- gsub('대관령', '강원', dat_cli_prov$station_name)

# 10. 파일 저장
# write.csv(dat_cli_prov, 'dat_cli_prov_강원.csv', row.names = F)
dat_cli_prov <- read.csv('dat_cli_prov_강원.csv', stringsAsFactors = F)

# 11. 기상데이터 년,월,일,보름,10일 나누기
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
cabbage_prod <- read.csv("노지가을배추(2001~2020).csv", stringsAsFactors=F)

# 2. 컬럼명 변경 
colnames(cabbage_prod) <- c('region','year','area_ha','prod_10a_kg','prod_ton')

# 3. 트랜드 값 

# fmla <- prod_10a_kg~year
# fit.lm <- lm(fmla, cabbage_prod)
# summary(fit.lm)

# 4. 강원도만 남겨두기
dat_prod <- cabbage_prod %>% filter(grepl('강원도',region)) %>% mutate(wide = ifelse(region%in%'강원도', substr(region, 1,2),
                                                                                       paste(substr(region,1,1), substr(region,3,3), sep=''))                                                                         )

prod_wide <- dat_prod %>% select(year, wide, prod_10a_kg) %>% spread(wide, prod_10a_kg, fill=NA)
# prod_wide$전국평균 <- round(apply(prod_wide[,2:ncol(prod_wide)], 1, mean),1)
# cor_prod <- round(cor(prod_wide[,c(2:ncol(prod_wide))]),3)



### 분석 시작 ###

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

# * 완전 고랭지 
# (P1) 파종 5/20 ~ 6/10 ----> 5월 3순기, 6월 1순기 (+0,1)            
# (P2) 정식 6/11 ~ 7/10 ----> 6월 2순기, 3순기,7월 1순기 (+2,3,4)          
# (P3) 결구기 7/11 ~ 8/10 --> 7월 2순기,3순기, 8월 1순기 (+5,6,7)  
# (P4) 수확기 8/11~ 8/20 ---> 8월 2순기 (+8)

# * 준고랭지 1기 작기형(7월 상/중순에 수확)
# (P1) 파종 4/1 ~ 4/31 ----> 4월 1순기, 2순기, 3순기 (+0,1,2)         
# (P2) 정식 5/1 ~ 5/20 ----> 5월 1순기, 2순기, 3순기 (+3,4,5)          
# (P3) 결구기 6/1 ~ 6/30 --> 6월 1순기, 2순기, 3순기 (+6,7,8)
# (P4) 수확기 7/1 ~ 20  ---> 7월 1순기, 2순기 (+9, 10)

# * 준고랭지 2기 작기형(추석 지나 10월에 수확)
# (P1) 파종 7/1 ~ 7/20 ----> 7월 1순기, 2순기, 3순기 (+0,1,2)           
# (P2) 정식 8/11~ 8/20 ----> 8월 2순기, 3순기 (+3,4)           
# (P3) 결구기 9/1 ~ 9/30 --> 9월 1순기, 2순기, 3순기 (+5,6,7)
# (P4) 수확기 10/1 ~ 10/31 > 10월 1순기, 2순기, 3순기 (+8,9,10)

# * 제주 월동작물
# (P1) 파종 8/10 ~ 9/10 ----> 8월 2순기, 8월 3순기, 9월 1순기 (+0,1,2)            
# (P2) 정식 9/11 ~ 9/30  ----> 9월 2순기, 9월 3순기 (+3,4)          
# (P3) 결구기 10/1 ~ 1/31 --> 10월 1순기, 10월 2순기, 10월 3순기, 11월 1순기, 11월 2순기, 11월 3순기, 12월 1순기, 12월 2순기, 12월 3순기, 1월 1순기, 1월 2순기, 1월 3순기 (+5,6,7,8,9,10,11,12,13,14,15,16)  
# (P4) 수확기 2월 ~ 3월 ---> 2월 1순기, 2월 2순기, 2월 3순기, 3월 1순기, 3월 2순기, 3월 3순기 (+17,18,19,20,21,22)

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

# 기준 
df <- dat_cli_prov; year_from <- 2001; month <- 7; ten_day <- 1; var_name <- 'tm_avg'; stat_name <- 'mean'
names(df)
names(df) <- c("date","wide","tm_avg","tm_min","tm_max","pr_day","hm_avg","ss_dura_sum","ws_max","year","month","day","ten_day","mon_third","tenday_no")

# 함수 
df_wide_func <- function(df, year_from, month, ten_day, var_name, stat_name){
  tenday_no0 <- (month-1)*3+ten_day
  tmp_df <- df %>% filter(year>=year_from)
  
  if(month<=4) {
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+2), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+3) & tenday_no<=(tenday_no0+5), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+6) & tenday_no<=(tenday_no0+8), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+9) & tenday_no<=(tenday_no0+10), 'P4', NA
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
  } else if(month>=8) { #제주도
    tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+2), 'P1',
                                                ifelse(tenday_no>=(tenday_no0+3) & tenday_no<=(tenday_no0+4), 'P2',
                                                       ifelse(tenday_no>=(tenday_no0+5) & tenday_no<=(tenday_no0+16), 'P3',
                                                              ifelse(tenday_no>=(tenday_no0+17) & tenday_no<=(tenday_no0+22), 'P4', NA
                                                              ))))) %>% filter(!is.na(period))
  }
  # else if(month>=8) { #남부지방
  #   tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+1), 'P1',
  #                                               ifelse(tenday_no>=(tenday_no0+2), 'P2',
  #                                                      ifelse(tenday_no>=(tenday_no0+3) & tenday_no<=(tenday_no0+7), 'P3',
  #                                                             ifelse(tenday_no>=(tenday_no0+8) & tenday_no<=(tenday_no0+11), 'P4', NA
  #                                                             ))))) %>% filter(!is.na(period))
  # }
  # else if(month>=8) { #중부지방
  #   tmp_df <- tmp_df %>% mutate(period = ifelse(tenday_no>=tenday_no0 & tenday_no<=(tenday_no0+1), 'P1',
  #                                               ifelse(tenday_no>=(tenday_no0+2), 'P2',
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
year_sel <- 2001; month_sel <- 7; ten_day_sel <- 1
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

dat_merge_7a_강원도 <- dat_merge 
dat_merge_7a_강원도 <- dat_merge_7a_강원도[!(dat_merge_7a_강원도$year == '2021'),]
write.csv(dat_merge_7a_강원도, 'dat_merge_7a_강원도.csv', row.names = F)
dat_merge <- read.csv('dat_merge_7a_강원도.csv', stringsAsFactors = F)
