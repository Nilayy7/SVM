#Prepare a classification model using SVM for salary data
library(kernlab)
library(ggplot2)
library(caret)
library(plyr)
install.packages("psych")
library(psych)

#Training Data
sal_train <- `SalaryData_Train(1)`
str(sal_train)
View(sal_train)
sal_train$educationno <- as.factor(sal_train$educationno)
class(sal_train)


#Test Data
sal_test <- `SalaryData_Test(1)`
str(sal_test)
View(sal_test)
sal_test$educationno <- as.factor(sal_test$educationno)
class(sal_test)

#Visualization
#EDA
ggplot(data = sal_train,aes(x=sal_train$Salary,y=sal_train$age,fill=sal_train$Salary))+geom_boxplot()+ggtitle("Box Plot")

plot(sal_train$workclass,sal_train$Salary)
plot(sal_train$education,sal_train$Salary)
plot(sal_train$educationno,sal_train$Salary)
plot(sal_train$maritalstatus,sal_train$Salary)
plot(sal_train$relationship,sal_train$Salary)
plot(sal_train$sex,sal_train$Salary)
plot(sal_train$native,sal_train$Salary)
ggplot(data=sal_train,aes(x=sal_train$Salary, y = sal_train$hoursperweek, fill = sal_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

#Density Plot 
ggplot(data=sal_train,aes(x = sal_train$age, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=sal_train,aes(x = sal_train$education, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=sal_train,aes(x = sal_train$sex, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=sal_train,aes(x = sal_train$native, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=sal_train,aes(x = sal_train$race, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

# Building model 
#kernel = vanilladot
model <- ksvm(sal_train$Salary~.,data=sal_train,kernel="vanilladot")
model
sal_pred <- predict(model,sal_test)
table(sal_pred,sal_test$Salary)
agreement <- sal_pred==sal_test$Salary
table(agreement)
prop.table(table(agreement))

#kernel = rbfdot
model1 <- ksvm(sal_train$Salary~.,data=sal_train,kernel="rbfdot")
model1
sal_pred1 <- predict(model1,sal_test)
table(sal_pred1,sal_test$Salary)
agreement1 <- sal_pred1 == sal_test$Salary
table(agreement1)
prop.table(table(agreement1))

#kernel = polydot
model2 <- ksvm(sal_train$Salary~.,data=sal_train,kernel="polydot")
model2
sal_pred2 <- predict(model2,sal_test)
table(sal_pred2,sal_test$Salary)
agreement2 <- sal_pred2 == sal_test$Salary
table(agreement2)
prop.table(table(agreement2))

