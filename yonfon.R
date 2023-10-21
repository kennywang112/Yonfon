library(tidyverse)
library(sf)
library(dummies)
train <- read_csv("/Users/wangqiqian/Desktop/永豐/new_training.csv")
train%>%group_by(縣市,鄉鎮市區)%>%summarize(平均單價 = sum(單價)/n())
#%>%
  #ggplot()+geom_col(aes(鄉鎮市區,平均單價))
