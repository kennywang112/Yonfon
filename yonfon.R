library(tidyverse)
library(psych)
library(mapproj)
library(ggmap)

train <- read_csv("C:/Users/USER/Yonfon/concat_data.csv")
origin <- read_csv("C:/Users/USER/Yonfon/30_Training Dataset_V2/training_data.csv")
price <- origin%>%select(單價)
train <- train[1:11751,]%>%cbind(price)

register_google('AIzaSyDkXQ0OwfJ9CXc4gUG7tb630puYEK0e0TM')
tw.map <- get_map(location = 'Taiwan', zoom = 7)
ggmap(tw.map) +
  geom_point(data = train, aes(x = lng, y = lat), color="purple")

train$`國中(下)` <- as.character(train$`國中(下)`)
train$`國中(中)` <- as.character(train$`國中(中)`)
train$`國中(高)` <- as.character(train$`國中(高)`)
train$`國小(明星)` <- as.character(train$`國小(明星)`)

train%>%ggplot()+geom_boxplot(aes(`國中(下)`, 單價))
train%>%ggplot()+geom_boxplot(aes(`國中(中)`, 單價))
train%>%ggplot()+geom_boxplot(aes(`國中(高)`, 單價))
train%>%ggplot()+geom_boxplot(aes(`國小(明星)`, 單價))
train%>%group_by(`國小(明星)`)%>%summarize(n())
train$`國小(明星)` <- case_when(
  train$`國小(明星)` == "0" | train$`國小(明星)` == "1" ~ "0",
  TRUE ~ "1"
)

train <- train%>%filter(單價 < 10)
train <- train%>%subset(主要建材 == "其他" & 單價 < 5 | 主要建材 != "其他")
train <- train%>%subset(主要建材 == "鋼筋混凝土加強磚造" & 單價 < 1.7 | 主要建材 != "鋼筋混凝土加強磚造")
train%>%ggplot()+geom_boxplot(aes(縣市, 單價))# 離群值
train%>%ggplot()+geom_boxplot(aes(使用分區, 單價))
train%>%ggplot()+geom_boxplot(aes(建物型態, 單價))
train%>%ggplot()+geom_boxplot(aes(主要建材, 單價))# 離群值
train%>%ggplot()+geom_boxplot(aes(主要用途, 單價))# 離群值
#train$移轉層次 <- as.character(train$移轉層次)
#train%>%ggplot()+geom_boxplot(aes(移轉層次, 單價))

max(train%>%filter(主要建材))








