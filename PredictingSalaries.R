#Building simple liner regression model

#Importing the dataset
dataset = read.csv("Predicting_Salaries.csv")

#Splitting our data set into training and testing sata set
#install.package('caTools)
library(caTools)
set.seed(123)
split = sample.split(dataset$AnnualSalary, SplitRatio = 3/4)
training_set = subset(dataset, split==TRUE)
testing_set=subset(dataset,split==FALSE)

# Fitting simple liner regression to training and testing sets
linearReg = lm(formula = AnnualSalary~YearsOfExperience,data = training_set)
summary(linearReg)

#Predicint testing set results
Y_pred = predict(linearReg,newdata=testing_set)
summary(Y_pred)

# Visualising the Training set results
#install.packages('ggplot2')
library(ggplot2)
#install.packages('scales')
library(scales)

ggplot() +
  geom_point(aes(x=training_set$YearsOfExperience, y=training_set$AnnualSalary),
             colour = 'red') +
  geom_line (aes( x= training_set$YearsOfExperience, y=predict(linearReg, newdata= training_set)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Training Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') +
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000)) 

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x=testing_set$YearsOfExperience, y=testing_set$AnnualSalary),
             colour = 'red') +
  geom_line (aes( x= training_set$YearsOfExperience, y=predict(linearReg, newdata= training_set)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Test Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary')+
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000)) 