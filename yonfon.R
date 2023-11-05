library(tidyverse)
library(psych)
library(mapproj)
library(ggmap)
library(fastDummies)

train <- read_csv("../Yonfon/concat_data.csv")
train <- read_csv("./desktop/永豐/concat_data.csv")
added <- read_csv("../Yonfon/training_quantity_distance.csv")
origin <- read_csv("../Yonfon/30_Training Dataset_V2/training_data.csv")
origin <- read_csv("./desktop/永豐/30_Training Dataset_V2/training_data.csv")
price <- origin%>%select(單價)

mean(origin$土地面積)
mean(origin$建物面積)
mean(origin$車位面積)
mean(origin$主建物面積)
mean(origin$陽台面積)
mean(origin$附屬建物面積)

for_test <- train[11752:nrow(train), ]

train <- train[1:11751,]%>%cbind(price)%>%select(-"縱坐標", -"橫坐標", -"geometry")
train$最近金融機構距離 <- added$最近金融機構距離
train$最近廟宇距離 <- added$最近廟宇距離

train$縣市%>%unique()
taipei_data <- train%>%filter(縣市 == "台北市")
gau_data <- train%>%filter(縣市 == "高雄市")
new_taipei_data <- train%>%filter(縣市 == "新北市")
tawyuan <- train%>%filter(縣市 == "桃園市")


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

train <- train%>%filter(單價 < 6)
train <- train%>%subset(`國中(中)` == 0 & 單價 < 3.4 | `國中(中)` != 0)
train <- train%>%subset(`國中(高)` == 2 & 單價 < 2.4 | `國中(高)` != 2)
train <- train%>%subset(`國小(明星)` == 1 & 單價 < 2.8 | `國小(明星)` != 1)
train <- train%>%subset(縣市 == "台南市" & 單價 > 0.55 | 縣市 != "台南市")
train <- train%>%subset(縣市 == "新北市" & 單價 < 3.15 | 縣市 != "新北市")
train <- train%>%subset(縣市 == "新北市" & 單價 > 0.75 | 縣市 != "新北市")
train <- train%>%subset(縣市 == "基隆市" & 單價 < 1.65 | 縣市 != "基隆市")
train <- train%>%subset(縣市 == "桃園市" & 單價 < 2.2 | 縣市 != "桃園市")
train <- train%>%subset(縣市 == "彰化縣" & 單價 < 1 | 縣市 != "彰化縣")
train <- train%>%subset(縣市 == "台北市" & 單價 < 5.7 | 縣市 != "台北市")
train <- train%>%subset(縣市 == "台北市" & 單價 > 1.8 | 縣市 != "台北市")
train <- train%>%subset(縣市 == "台中市" & 單價 < 2.45 | 縣市 != "台中市")
train <- train%>%subset(縣市 == "高雄市" & 單價 < 2.2 | 縣市 != "高雄市")
train <- train%>%subset(縣市 == "嘉義市" & 單價 < 0.95 | 縣市 != "嘉義市")
train <- train%>%subset(縣市 == "桃園市" & 單價 > 0.5 | 縣市 != "桃園市")
train <- train%>%subset(縣市 == "宜蘭縣" & 單價 < 1.9 | 縣市 != "宜蘭縣")
train <- train%>%subset(縣市 == "高雄市" & 單價 > 0.3 | 縣市 != "高雄市")
train <- train%>%subset(縣市 == "高雄市" & 單價 < 2.1 | 縣市 != "高雄市")
train <- train%>%subset(縣市 == "新北市" & 單價 > 0.7 | 縣市 != "新北市")
train <- train%>%subset(主要建材 == "其他" & 單價 < 4.5 | 主要建材 != "其他")
train <- train%>%subset(主要建材 == "鋼筋混凝土造" & 單價 < 3.5 | 主要建材 != "鋼筋混凝土造")
train <- train%>%subset(主要建材 == "加強磚造" & 單價 < 2.9 | 主要建材 != "加強磚造")
train <- train%>%subset(主要建材 == "加強磚造" & 單價 > 1.1 | 主要建材 != "加強磚造")
train <- train%>%subset(主要建材 == "鋼骨造" & 單價 < 4.4 | 主要建材 != "鋼骨造")
train <- train%>%subset(主要建材 == "鋼筋混凝土加強磚造" & 單價 < 1.7 | 主要建材 != "鋼筋混凝土加強磚造")
train <- train%>%subset(主要用途 == "集合住宅" & 單價 < 9 | 主要用途 != "集合住宅")
train <- train%>%subset(主要用途 == "住商用" & 單價 < 2 | 主要用途 != "住商用")
train <- train%>%subset(主要用途 == "一般事務所" & 單價 < 7.2 | 主要用途 != "一般事務所")
train <- train%>%subset(主要用途 == "辦公室" & 單價 < 4 | 主要用途 != "辦公室")
train <- train%>%subset(主要用途 == "辦公室" & 單價 > 1 | 主要用途 != "辦公室")
train <- train%>%subset(主要用途 == "其他" & 單價 < 3.5 | 主要用途 != "其他")
train <- train%>%subset(主要用途 == "集合住宅" & 單價 < 3.4 | 主要用途 != "集合住宅")
train <- train%>%subset(主要用途 == "住家用" & 單價 < 4.5 | 主要用途 != "住家用")
train <- train%>%subset(主要用途 == "國民住宅" & 單價 < 1.6 | 主要用途 != "國民住宅")
train <- train%>%subset(主要建材 == "加強磚造" & 單價 < 3.8 | 主要建材 != "加強磚造")
train <- train%>%subset(主要建材 == "加強磚造" & 單價 < 3.8 | 主要建材 != "加強磚造")
train <- train%>%subset(主要建材 == "其他" & 單價 < 4.5 | 主要建材 != "其他")
train <- train%>%subset(建物型態 == "住宅大樓(11層含以上有電梯)" & 單價 < 3.35 | 建物型態 != "住宅大樓(11層含以上有電梯)")
train <- train%>%subset(建物型態 == "公寓(5樓含以下無電梯)" & 單價 < 3.55 | 建物型態 != "公寓(5樓含以下無電梯)")
train <- train%>%subset(建物型態 == "透天厝" & 單價 < 2 | 建物型態 != "透天厝")
train <- train%>%subset(建物型態 == "透天厝" & 單價 > 1.5 | 建物型態 != "透天厝")
train <- train%>%subset(建物型態 == "華廈(10層含以下有電梯)" & 單價 < 3.8 | 建物型態 != "華廈(10層含以下有電梯)")
train <- train%>%subset(使用分區 == "商" & 單價 < 5.5 | 使用分區 != "商")
train <- train%>%subset(使用分區 == "商" & 單價 > 1 | 使用分區 != "商")
train <- train%>%subset(使用分區 == "農" & 單價 < 1.7 | 使用分區 != "農")
train <- train%>%subset(使用分區 == "其他" & 單價 < 2.4 | 使用分區 != "其他")
train <- train%>%subset(使用分區 == "None" & 單價 < 3.57 | 使用分區 != "None")

train$建物型態
train%>%ggplot()+geom_boxplot(aes(縣市, 單價))# 離群值
train%>%ggplot()+geom_boxplot(aes(使用分區, 單價))
train%>%ggplot()+geom_boxplot(aes(建物型態, 單價))
train%>%ggplot()+geom_boxplot(aes(主要建材, 單價))# 離群值
train%>%ggplot()+geom_boxplot(aes(主要用途, 單價))# 離群值
#train$移轉層次 <- as.character(train$移轉層次)
#train%>%ggplot()+geom_boxplot(aes(移轉層次, 單價))

numeric_train <- train%>%select(-c("縣市","鄉鎮市區","路名","使用分區","主要用途","主要建材","建物型態","備註"))
# 最近火車站 土地面積 總樓層數 國小(明星) 國中(下) 最近傳統市場 最近加油站 最近金融機構 最近娛樂設施
model <- lm(formula = 單價 ~., data = numeric_train)
full<-formula(model)
summary(model)
# 檢驗殘差常態性
shapiro.test(model$residual[1:1000])

model0<-lm(單價 ~ 1, data = numeric_train)
#stepwise regression
step(model0,direction = "both",scope = full)

train%>%filter(土地面積 < 2.5)%>%ggplot()+geom_point(aes(土地面積, 單價,color = 縣市))
train%>%filter(土地面積 < 2.5)%>%summarize(n())
train%>%group_by(縣市, 總樓層數)%>%summarize(avg = sum(單價)/n())%>%filter(總樓層數<40)%>%
  ggplot()+geom_col(aes(總樓層數, avg,fill = 縣市), position = "dodge")+coord_flip()

train%>%group_by(移轉層次)%>%summarize(avg = sum(單價)/n())%>%
  ggplot()+geom_col(aes(移轉層次, avg))

write.csv(train, "C:/Users/USER/Yonfon/new_concat_data.csv", row.names=FALSE)

























