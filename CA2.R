#loading the data
data1 <- read.csv("C:/Users/bhara/OneDrive/Desktop/ISEQOverallHistorical.csv" ,header = TRUE)
data2 <- read.csv("C:/Users/bhara/OneDrive/Desktop/test.csv" ,header = TRUE)
#merging data in one data frame
library(dplyr)
library(readr)
data=full_join(data1,data2)
View(data)

#Replacing NA with mean values ignorning the outlier

summary(data$ï..Article_Published)
data3=data$ï..Article_Published
data1=na.omit(data$ï..Article_Published)
summary(data1)
bench=6+1.5*IQR(data1) # getting outlier using third quartile
data2=data1[data1<bench]
summary(data2)
mea=mean(data2)
data3[is.na(data3)]=mea
summary(data3)
bench=5+1.5*IQR(data3)
data3[data1<bench]=mea
summary(data3)
View(data2)
mean(data1)
#Defining independent and dependent variables
y=data$Price_ISEQ
x1=data$Vol_Mil_ISEQ
x2=data$ChangePercent_ISEQ
x3=data3
x4=data$High_ISEQ
x5=data$Low_ISEQ
x6=data$Open_ISEQ

# creating data frame
df=na.omit(data.frame(x1,x2,x3,x4,x5,x6,y))
#fitting the model
fit = glm(y ~., data=df, family='gaussian')
summary(fit)

#
#split the data into 80% as a trainset and 20% as a testset
n=nrow(df)
indexes = sample(n,n*(80/100))
trainset = df[indexes,]
testset = df[-indexes,]
#Fit the full model
actual=testset$y
full.model <- glm(trainset$y ~., data = trainset, family='gaussian')
summary(full.model)
yhat=predict(full.model, testset[1:6])
rmse_f=sqrt((sum(yhat-actual)^2)/(nrow(testset)))
#Reduced Model
library(MASS)

reduced.model=stepAIC(full.model)
yhat_r=predict(reduced.model, testset[1:6])
rmse_r=sqrt((sum(yhat_r-actual)^2)/(nrow(testset)))
rmse_f
rmse_r
# fit the model using trainset
#trainset.glm <- glm(trainset$y ~.,trainset, family="binomial") # ~. shows that we include all ind. variables

#Predictive Model
phat_i=predict(full.model , testset, type='response')
# confusion matrix and accuracy 
actual=testset$y
conf_mat=table(pred,actual)
RMSE=c(0,0)
RMSE=RMSE+c(rmse_f,rmse_r)
