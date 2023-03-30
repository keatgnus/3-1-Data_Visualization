# package
library(ggplot2)
library(dplyr)

# 파일 불러오기
data_file <- choose.files()
data_file

# csv읽기, 데이터 정보
ncdc_normals <- read.csv(data_file)
ncdc_normals %>% dim()
ncdc_normals %>% head()
ncdc_normals %>% summary()
ncdc_normals %>% sapply(class)
ncdc_normals %>% sapply(typeof)

# cha형식에서 date형식 변환
ncdc_normals$date <- ncdc_normals$date%>% as.Date("%Y-%m-%d")
ncdc_normals %>% sapply(class)
ncdc_normals %>% sapply(typeof)

# station_id 종류 개수
ncdc_normals$station_id %>% unique() %>% length()

# station_id 뽑고, location 정해주고 df 생성
station_loc <- data.frame(station_id = c("USW00014819","USC00042319","USW00093107","USW00012918"),
                          location = c("Chicago","Death Valley","San Diego","Houston"))
station_loc

# station_id로 inner_join
temps_long <- ncdc_normals %>% inner_join(station_loc, by="station_id")
temps_long %>% head()
temps_long %>% dim()

# temps long data type : Cha > Date
temps_long$date <- temps_long$date %>% as.date('%Y-%m-%d')

# x축에 표시할 눈금
date_s <- '0000-01-01' %>% as.Date('%Y-%m-%d')
date_e <- '0001-01-01' %>% as.Date('%Y-%m-%d')
break_date <- seq(date_s, date_e, by = '3 month')
break_date

# ggplot + 축 설정 
ggplot(temps_long, aes(x=date, y=temperature, color=location)) +
  geom_line() +
  scale_x_date(name = 'month',
               breaks = break_date,
               labels = c('Jan','Apr','Jul','Oct','Jan')) +
  scale_y_continuous(name = 'temp', limits = c(0,110)) + # continuous 연속형 (온도)
  #ylab('Temp') +
  labs(title = 'Fig. 2.3', subtitle = 'Daily temperature normals') +
  theme_light()

### 예제 - 기상철 자료 2021년

# csv 파일 불러오기
data_2021 <- read.csv('OBS_ASOS_DD_20220308125952.csv', fileEncoding = 'cp949')
data_2021 %>% dim()
data_2021 %>% head()

# 자료형 확인
data_2021 %>% sapply(class)

# 일시를 date 형식으로 변환
data_2021$일시 <- data_2021$일시 %>% as.Date('%Y-%m-%d')
data_2021 %>% sapply(class)

# 기초통계량
data_2021 %>% summary()

# 지점명 확인
data_2021$지점명 %>% table()
data_2021_use <- data_2021 %>% filter(지점명 %in% c('대전',
                                                 '서울','세종','제주'))
data_2021_use

data_2021 %>% names()
data_2021_month <- data_2021 %>%
  mutate(month = format(일시, '%B')) %>%
  group_by(지점명, month) %>%
  summarise(mean = mean(평균기온..C.)) %>%
  ungroup() %>%
  mutate(month = factor(month,levels = paste(1:12, '월',sep = '') ))
  

data_2021_month

# 최종 ggplot + geom_tile + fill color
ggplot(data_2021_month, aes(x = month, y = 지점명, fill = mean)) +
  geom_tile(width = .95, height = 0.95) +
  scale_fill_viridis_c(option = 'B', begin = 0.15, end =  0.98,
                       name = 'temperature') +
  coord_fixed(expand = FALSE) +
  ylab(NULL) #ylab('')


# x축에 표시할 눈금
date_s2 <- '2021-01-01' %>% as.Date('%Y-%m-%d')
date_e2 <- '2022-01-01' %>% as.Date('%Y-%m-%d')
break_date2 <- seq.Date(date_s2, date_e2, by='3 month')
break_date2
date_lab <- format(break_date2, '%B')
date_lab

# ggplot + 축 설정
data_2021 %>% names()
ggplot(data_2021, aes(x=일시, y=평균기온..C., color=지점명)) +
  geom_smooth(linewidth = 1,se=F,span=0.2) + # linewidth (size), geom_line → smooth 가능
  scale_x_date(name = '월',                  # se : 테두리, span : 스무스 정도
               breaks = break_date2,
               labels = date_lab) +
  scale_y_continuous(name = '평균기온') +
  theme_light()

### fig 2.4
# 월 평균
temps_long %>% head()
temps_long %>% names()

mean_temps <- temps_long %>%
  group_by(location, month) %>%
  summarise(mean = mean(temperature)) %>%
  ungroup() %>%
  mutate(month = factor(month %>% paste(), # factor 이용해 문자로 바꾼 후 level 지정
                        levels = 1:12 %>% paste())) #ungroup으로 마무리해보기

mean_temps$month %>% levels()
mean_temps

# 기본
ggplot(mean_temps, aes(x=month, y=location, fill=mean)) +
  geom_tile(width= .95, height = 0.95)

# 기본 + coord_fix (직사각형) + 컬러스케일(+ 밝기 조정)
ggplot(mean_temps, aes(x=month, y=location, fill=mean)) +
  geom_tile(width= .95, height = 0.95) +
  coord_fixed(expand = FALSE) +
  scale_fill_viridis_c(option = 'B', begin = 0.15, end = 0.78)

# 최종 ggplot + geom_tile + fill color
ggplot(mean_temps, aes(x = month, y = location, fill = mean)) +
  geom_tile(width = .95, height = 0.95) +
  scale_fill_viridis_c(option = 'B', begin = 0.15, end =  0.98,
                       name = 'temperature') +
  coord_fixed(expand = FALSE) +
  ylab(NULL) #ylab('')




