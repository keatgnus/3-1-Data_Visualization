# 패키지 설치
install.packages('ggplot2')
install.packages('dyplr')
library(ggplot2)
library(dplyr)

# 데이터 : mpg 데이터 셋
mpg
View(mpg)
data_raw <- read.csv('http://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/mpg.csv')
data_raw

data_raw %>% dim()
data_raw %>% head()
data_raw %>% summary()

data_use <- data_raw %>% select(-1)
data_use %>% dim()
data_use %>% head()

ggplot(data_use, aes(x=displ,y=hwy)) + geom_point()
ggplot(data_use) + geom_point(aes(x=displ,y=hwy))
ggplot() + geom_point(aes(x=displ, y=hwy), data=data_use)

ggplot(data_use, aes(x=displ,y=hwy)) + geom_smooth()
ggplot(data_use, aes(x=displ,y=hwy)) + geom_smooth() + geom_point()

ggp <- ggplot(data_use, aes(x=displ,y=hwy)) + geom_point()
ggp + geom_smooth()
