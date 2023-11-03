library(tidyverse)

train <- read_csv("./new_concat_data.csv")

train%>%head()
train$縣市%>%unique()

city_mapping <- data.frame(
  縣市 = c("台北市", "高雄市", "新北市", "桃園市", "台中市", "台南市", "苗栗縣", "新竹縣", "基隆市", "屏東縣", "新竹市", "宜蘭縣", "花蓮縣", "嘉義市", "金門縣", "嘉義縣", "彰化縣", "雲林縣"),
  English = c("Taipei", "Kaohsiung", "New_Taipei", "Taoyuan", "Taichung", "Tainan", "Miaoli", "Hsinchu_City", "Keelung", "Pingtung", "Hsinchu_County", "Yilan", "Hualien", "Chiayi", "Kinmen", "Chiayi", "Changhua", "Yunlin"))

for (i in 1:nrow(city_mapping)) {
  city_name <- as.character(city_mapping[i, "縣市"])
  city_english <- as.character(city_mapping[i, "English"])
  
  filtered_data <- train %>% filter(縣市 == city_name)
  write.csv(filtered_data, paste0("C:/Users/USER/Yonfon/縣市分群/", city_english, ".csv"), row.names = FALSE)
}














































