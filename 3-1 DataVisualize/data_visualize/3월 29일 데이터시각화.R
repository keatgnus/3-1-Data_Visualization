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

# station_id로 inner_join
temps_long <- ncdc_normals %>% inner_join(station_loc, by="station_id")
temps_long %>% head()
temps_long %>% dim()

# fugure 3.2
install.packages("cowplot")
library(cowplot)

# filter: houston
data_Houston <- temps_long %>% filter(location == 'Houston')
data_Houston %>% dim()
data_Houston %>% head()

# x축에 표시할 눈금
date_s <- '0000-01-01' %>% as.Date('%Y-%m-%d')
date_e <- '0001-01-01' %>% as.Date('%Y-%m-%d')
break_date <- seq(date_s, date_e, by = '3 month')
break_date

# ggplot + 축 설정
temp_plot <- ggplot(data_Houston, aes(x=date, y=temperature)) +
  geom_line(linewidth=1, color='royalblue') +
  scale_x_date(name='month',
               breaks=break_date,
               labels=c('Jan','Apr','Jul','Oct','Jan')) +
  scale_y_continuous(name = 'temp') +
  theme_light()
temp_plot

plot_ab <- plot_grid(temp_plot,
                     temp_plot,
                     nrow=1,
                     rel_eidths = c(1,2),
                     labels = c('a','b'))

plot_abc <- plot_grid(plot_ab,
                     temp_plot,
                     ncol=1,
                     rel_heights = c(1.5, 1),
                     labels = c('','c'))

plot_abc


# figure 3.5 ~ 3.6
#install.packages("ggrepel")
library(ggrepel) # geom_text_repel()

# read csv file
Us_census <- read.csv(choose.files())
Us_census %>% dim()
Us_census %>% head()
Us_census %>% names()

tx_counties <- Us_census %>%
  filter(state == 'Texas') %>%
  select(name, pop2010) %>%
  mutate(county = gsub('County','', name),
         popratio = pop2010/median(pop2010)) %>%
  arrange(pop2010 %>% desc()) %>%
  mutate(index = 1:n(),
         label = ifelse(index<=3 | index>n()-3 | runif(n())<0.04, county,''))

tx_counties %>% head()
tx_counties %>% tail()

# figure 3.6
ggplot(tx_counties, aes(x=index, y=popratio)) +
  geom_point(size=1, color='royalblue') +
  geom_text_repel(aes(label=label),
                  min.segment.length = 0,
                  max.overlaps = 100)
# figure 3.5
ggplot(tx_counties, aes(x=index, y=popratio)) +
  geom_point(size=1, color='royalblue') +
  geom_text_repel(aes(label=label),
                  min.segment.length = 0,
                  max.overlaps = 100)
