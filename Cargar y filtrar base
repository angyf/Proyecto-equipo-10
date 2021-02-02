library(dplyr)
library(tidyr)

setwd("/Users/...")


test <- read.csv("test.csv")
train <- read.csv("train.csv")
dim(test) # 16 columnas
dim(train) #17 columnas
str(test)
str(train)
summary(test)
summary(train)

head(train) 

train1 <- train %>%  #Train1 es la base con los pacientes con el defecto cardiaco
  filter(TenYearCHD==1) # con filter tenemos una base con los pacientes que padecen el defecto cardiaco

train1
dim(train1)

trainF <- train1 %>%
  filter(sex=="F") 

trainM <- train1 %>%
  filter(sex=="M") 

testM <- test %>%
  filter(sex=="M") 
testF <- test %>%
  filter(sex=="F") 

#Diferencias entre poblaciones respecto a niveles de BMI

t.test(train1$BMI~train1$sex) 
t.test(test$BMI~test$sex) 
t.test(train1$BMI,test$BMI) #sig
t.test(train1$BMI,train2$BMI) #sig.
t.test(train2$BMI,test$BMI) #no sig

hist(train1$BMI)
hist(train2$BMI)
hist(test$BMI)

#Diferencias entre poblaciones respecto a niveles de totChol

t.test(train1$totChol~train1$sex) #sig
t.test(trainF$totChol,testF$totChol) #sig
t.test(trainM$totChol,testM$totChol) #sig
t.test(test$totChol~test$sex) #sig
t.test(train1$totChol,test$totChol) #sig
t.test(train1$totChol,train2$totChol) #sig
t.test(train2$totChol,test$totChol) #no sig

hist(train1$totChol)
hist(train2$totChol)
hist(test$totChol)
hist(trainF$totChol)
hist(trainM$totChol)
hist(testF$totChol)
hist(testM$totChol)

#Diferencias entre poblaciones respecto a hearRate

t.test(train1$heartRate~train1$sex) 
t.test(trainF$heartRate,testF$heartRate)
t.test(trainM$heartRate,testM$heartRate)
t.test(test$heartRate~test$sex) #sig
t.test(train1$heartRate,test$heartRate)
t.test(train1$heartRate,train2$heartRate)
t.test(train2$heartRate,test$heartRate)

hist(train1$heartRate)
hist(train2$heartRate)
hist(test$heartRate)
hist(trainF$heartRate)
hist(trainM$heartRate)
hist(testF$heartRate)
hist(testM$heartRate)

#Diferencias entre poblaciones respecto a niveles de glucose

t.test(train1$glucose~train1$sex) 
t.test(trainF$glucose,testF$glucose) #sig
t.test(trainM$glucose,testM$glucose) #sig
t.test(test$glucose~test$sex) 
t.test(train1$glucose,test$glucose) #sig
t.test(train1$glucose,train2$glucose) #sig
t.test(train2$glucose,test$glucose) #no

hist(train1$glucose)
hist(train2$glucose)
hist(test$glucose)
hist(trainF$glucose)
hist(trainM$glucose)
hist(testF$glucose)
hist(testM$glucose)

wilcox.test(train1$glucose~train1$sex) 
wilcox.test(trainF$glucose,testF$glucose) #sig
wilcox.test(trainM$glucose,testM$glucose) 
wilcox.test(test$glucose~test$sex) 
wilcox.test(train1$glucose,test$glucose) #sig
wilcox.test(train1$glucose,train2$glucose) #sig
wilcox.test(train2$glucose,test$glucose) #no


#Diferencias entre poblaciones respecto a valores de diaBP

t.test(train1$diaBP~train1$sex) 
t.test(trainF$diaBP,testF$diaBP) #sig
t.test(trainM$diaBP,testM$diaBP) #sig
t.test(test$diaBP~test$sex) 
t.test(train1$diaBP,test$diaBP) #sig
t.test(train1$diaBP,train2$diaBP) #sig
t.test(train2$diaBP,test$diaBP) #no

hist(train1$diaBP)
hist(train2$diaBP)
hist(test$diaBP)
hist(trainF$diaBP)
hist(trainM$diaBP)
hist(testF$diaBP)
hist(testM$diaBP)


#Diferencias entre poblaciones respecto a valores de SysBP

t.test(train1$sysBP~train1$sex) #sig
t.test(trainF$sysBP,testF$sysBP) #sig
t.test(trainM$sysBP,testM$sysBP) #sig
t.test(test$sysBP~test$sex) 
t.test(train1$sysBP,test$sysBP) #sig
t.test(train1$sysBP,train2$sysBP) #sig
t.test(train2$sysBP,test$sysBP) #no

hist(train1$sysBP)
hist(train2$sysBP)
hist(test$sysBP)
hist(trainF$sysBP)
hist(trainM$sysBP)
hist(testF$sysBP)
hist(testM$sysBP)

train1
dim(train1)

summary(train1)
summary(train2)

train2 <- train %>%
  filter(TenYearCHD==0) 

train2
dim(train2)

summary(train2)
