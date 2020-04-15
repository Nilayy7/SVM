#The fireforest data set is being used for Support vector machine modelling by using the factors like wind ,rain, rh and temperature etc and 
#predict the size of burned area.
install.packages("caret")
install.packages("kernlab")
library(caret)
library(kernlab)
library(plyr)
ff <- forestfires

#EDA 
#Histogram
hist(ff$area)
rug(ff$area)
boxplot(ff$area)
#We come to a point that the area has lot of 0's

# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :
normalize  <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

ff$temp = normalize(ff$temp)
ff$rain = normalize(ff$rain)
ff$wind = normalize(ff$wind)
ff$RH =  normalize(ff$RH)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :
# Transform the Area value to Y 
n <- mutate(ff, y = log(area + 1))  # default is to the base e, y is lower case
hist(n$y)
attach(ff)
ff_train <- ff[1:370,]
ff_test <- ff[371:517,]

plot(ff_train$month,ff_train$size_category)
plot(ff_train$temp,ff_train$size_category)
plot(ff_train$RH,ff_train$size_category)
plot(ff_train$wind,ff_train$size_category)
plot(ff_train$rain,ff_train$size_category)

#Boxplot
ggplot(data=ff_train,aes(x=ff_train$size_category, y = ff_train$month, fill = ff_train$size_category)) +
  geom_boxplot() +
  ggtitle("Box Plot")

#Density Plot
ggplot(data=ff_train,aes(x=ff_train$size_category, y = ff_train$wind, fill = ff_train$size_category))+
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=ff_train,aes(x=ff_train$size_category, y = ff_train$temp, fill = ff_train$size_category))+
  geom_density(alpha = 0.9, color = 'Violet')

# kvsm() function uses gaussian RBF kernel 
# Building model 
ff_model <- ksvm(size_category~temp+rain+RH+wind,data=ff_train,kernel="vanilladot")
ff_model
#Training error : 0.243243

area_pred <- predict(ff_model,ff_test)
table(area_pred,ff_test$size_category)

agreement <- area_pred == ff_test$size_category
table(agreement)
prop.table(table(agreement))

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

#kernel = rbfplot
ff_model1 <- ksvm(size_category~temp+rain+RH+wind,data=ff_train,kernel="rbfdot")
ff_model1
area_pred1 <- predict(ff_model1,newdata=ff_test)
mean(area_pred1==ff_test$size_category) #0.6530

agreement1 <- area_pred1 == ff_test$size_category
table(agreement1)
prop.table(table(agreement1))

#kernel = besseldot
ff_model2 <- ksvm(size_category~temp+rain+RH+wind,data=ff_train,kernel="besseldot")
area_pred2 <- predict(ff_model2,newdata=ff_test)
mean(area_pred2==ff_test$size_category) #0.666

agreement2 <- area_pred2 == ff_test$size_category
table(agreement2)
prop.table(table(agreement2))

##kernel = polydot
ff_model3 <- ksvm(size_category~temp+rain+RH+wind,data=ff_train,kernel="polydot")
area_pred3 <- predict(ff_model3,newdata=ff_test)
mean(area_pred3==ff_test$size_category)

agreement3 <- area_pred3 == ff_test$size_category
table(agreement3)
prop.table(table(agreement3))
