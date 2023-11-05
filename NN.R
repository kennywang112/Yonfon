library(keras)
library(tidyverse)

x_train <- read_csv("./desktop/永豐/neural_network/X_train.csv")
y_train <- read_csv("./desktop/永豐/neural_network/Y_train.csv")
x_test <- read_csv("./desktop/永豐/neural_network/X_test.csv")
y_test <- read_csv("./desktop/永豐/neural_network/Y_test.csv")

model<-keras_model_sequential()%>%
  layer_dense(16,activation = 'relu',input_shape = c(7))%>%
  layer_dense(16,activation = 'relu')%>%
  layer_dense(2,activation='linear')

model %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)
history<-model%>%fit(
  x_train,y_train,epochs=100,batch_size=50
)
plot(history)