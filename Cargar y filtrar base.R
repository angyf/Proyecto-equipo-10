library(dplyr)
library(tidyr)

setwd("/Users/...")


test <- read.csv("test.csv")
train <- read.csv("train.csv")


head(train) #BD ORIGINAL DE PACIENTES DE REFERENCIA
head(test) #BD ORIGINAL DE PACIENTES DE PRUEBA

train1 <- train %>%  #Train1 es la base con los pacientes de referecia CON el defecto cardiaco
  filter(TenYearCHD==1) 

train2 <- train %>%   #Train2 es la base con los pacientes de referecia SIN el defecto cardiaco
  filter(TenYearCHD==0) 

trainF <- train1 %>% #TrainF es la base con las pacientes MUJERES de referecia con el defecto cardiaco
  filter(sex=="F") 

trainM <- train1 %>%  #TrainM es la base con los pacientes HOMBRES de referecia con el defecto cardiaco
  filter(sex=="M") 

testM <- test %>%  #TestM es la base con los pacientes HOMBRES de prueba 
  filter(sex=="M") 

testF <- test %>% #TestF es la base con las pacientes MUJERES de prueba 
  filter(sex=="F") 

summary(test)
summary(train)

#Diferencias entre poblaciones respecto a niveles de BMI

t.test(train$BMI~train$sex)#sig < PACIENTES DE REF, DIF EN Índice de masa corporal RESPECTO AL SEXO
t.test(train1$IBM~train1$sex)# PACIENTES DE REF CON DC, DIF EN Índice de masa corporal RESPECTO AL SEXO
t.test(train2$BMI~train2$sex)#sig < PACIENTES DE REF SIN DC, DIF EN Índice de masa corporal RESPECTO AL SEXO
t.test(test$BMI~test$sex)# PACIENTES DE PRUEBA, DIF EN Índice de masa corporal RESPECTO AL SEXO

t.test(train$BMI,test$BMI) # PACIENTES DE REF vs PACIENTES PRUEBA
t.test(train1$BMI,test$BMI) #sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
t.test(train1$BMI,train2$BMI) #sig < PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
t.test(train2$BMI,test$BMI) #no sig < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA

hist(train$BMI) 
hist(train1$BMI)
hist(train2$BMI)
hist(test$BMI)

#Diferencias entre poblaciones respecto a niveles de totChol

t.test(train$totChol~train$sex)#sig < PACIENTES DE REF, DIF EN Coloesterol RESPECTO AL SEXO
t.test(train1$totChol~train1$sex)#sig < PACIENTES DE REF CON DC, DIF EN Colesterol RESPECTO AL SEXO
t.test(train2$totChol~train2$sex)#sig < PACIENTES DE REF SIN DC, DIF EN Colesterol RESPECTO AL SEXO
t.test(test$totChol~test$sex) #sig < PACIENTES DE PRUEBA, DIF EN Colesterol RESPECTO AL SEXO

t.test(trainF$totChol,testF$totChol) #sig PACIENTES DE REF CON DC MU vs PACIENTES DE PRUEBA MUJ
t.test(trainM$totChol,testM$totChol) #sig PACIENTES DE REF CON DC H vs PACIENTES DE PRUEBA H

t.test(train$totChol,test$totChol)# PACIENTES DE REF vs PACIENTES PRUEBA
t.test(train1$totChol,test$totChol) #sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
t.test(train1$totChol,train2$totChol) #sig < PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
t.test(train2$totChol,test$totChol) #no sig < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA

hist(train$totChol)
hist(train1$totChol)
hist(train2$totChol)
hist(test$totChol)
hist(trainF$totChol)
hist(trainM$totChol)
hist(testF$totChol)
hist(testM$totChol)

#Diferencias entre poblaciones respecto a hearRate

t.test(train$heartRate~train$sex)#sig < PACIENTES DE REF, DIF EN Frec cardiaca RESPECTO AL SEXO
t.test(train1$heartRate~train1$sex)# PACIENTES DE REF CON DC, DIF EN Frec cardiaca RESPECTO AL SEXO
t.test(train2$heartRate~train2$sex)#sig < PACIENTES DE REF SIN DC, DIF EN Frec cardiaca RESPECTO AL SEXO
t.test(test$heartRate~test$sex) #sig < PACIENTES DE PRUEBA, DIF EN Frec cardiaca RESPECTO AL SEXO

t.test(trainF$heartRate,testF$heartRate)# PACIENTES DE REF CON DC MU vs PACIENTES DE PRUEBA MUJ
t.test(trainM$heartRate,testM$heartRate)# PACIENTES DE REF CON DC H vs PACIENTES DE PRUEBA H

t.test(train$heartRate,test$heartRate)# PACIENTES DE REF vs PACIENTES PRUEBA
t.test(train1$heartRate,test$heartRate)# PACIENTES DE REF CON DC vs PACIENTES PRUEBA
t.test(train1$heartRate,train2$heartRate)# PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
t.test(train2$heartRate,test$heartRate)# PACIENTES DE REF SIN DC vs PACIENTES PRUEBA

hist(train$heartRate)
hist(train1$heartRate)
hist(train2$heartRate)
hist(test$heartRate)
hist(trainF$heartRate)
hist(trainM$heartRate)
hist(testF$heartRate)
hist(testM$heartRate)

#Diferencias entre poblaciones respecto a niveles de glucose

hist(train$glucose)
hist(train1$glucose)
hist(train2$glucose)
hist(test$glucose)
hist(trainF$glucose)
hist(trainM$glucose)
hist(testF$glucose)
hist(testM$glucose)

wilcox.test(train$glucose~train$sex)# PACIENTES DE REF, DIF EN Glucosa RESPECTO AL SEXO
wilcox.test(train1$glucose~train1$sex)# PACIENTES DE REF CON DC, DIF EN Glucosa RESPECTO AL SEXO 
wilcox.test(train2$glucose~train2$sex)# PACIENTES DE REF SIN DC, DIF EN Glucosa RESPECTO AL SEXO
wilcox.test(test$glucose~test$sex)# PACIENTES DE PRUEBA, DIF EN Glucosa RESPECTO AL SEXO 

wilcox.test(trainF$glucose,testF$glucose)#sig > PACIENTES DE REF CON DC MU vs PACIENTES DE PRUEBA MUJ
wilcox.test(trainM$glucose,testM$glucose)# PACIENTES DE REF CON DC H vs PACIENTES DE PRUEBA H

wilcox.test(train$glucose,test$glucose)# PACIENTES DE REF vs PACIENTES PRUEBA
wilcox.test(train1$glucose,test$glucose)#sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
wilcox.test(train1$glucose,train2$glucose) #sig < PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
wilcox.test(train2$glucose,test$glucose) #no < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA


#Diferencias entre poblaciones respecto a valores de diaBP

t.test(train$diaBP~train$sex)# sig < PACIENTES DE REF, DIF EN Presión diastólica RESPECTO AL SEXO
t.test(train1$diaBP~train1$sex)# PACIENTES DE REF CON DC, DIF EN Presión diastólica RESPECTO AL SEXO  
t.test(train2$diaBP~train2$sex)# sig < PACIENTES DE REF SIN DC, DIF EN Presión diastólica RESPECTO AL SEXO 
t.test(test$diaBP~test$sex)# PACIENTES DE PRUEBA, DIF EN Presión diastólica RESPECTO AL SEXO 

t.test(trainF$diaBP,testF$diaBP) #sig > PACIENTES DE REF CON DC MU vs PACIENTES DE PRUEBA MUJ
t.test(trainM$diaBP,testM$diaBP) #sig < PACIENTES DE REF CON DC H vs PACIENTES DE PRUEBA H

t.test(train$diaBP,test$diaBP)# PACIENTES DE REF vs PACIENTES PRUEBA 
t.test(train1$diaBP,test$diaBP)#sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
t.test(train1$diaBP,train2$diaBP)#sig < PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
t.test(train2$diaBP,test$diaBP)#no < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA

hist(train$diaBP)
hist(train1$diaBP)
hist(train2$diaBP)
hist(test$diaBP)
hist(trainF$diaBP)
hist(trainM$diaBP)
hist(testF$diaBP)
hist(testM$diaBP)


#Diferencias entre poblaciones respecto a valores de SysBP

t.test(train$sysBP~train$sex)# PACIENTES DE REF, DIF EN Presión sistólica RESPECTO AL SEXO
t.test(train1$sysBP~train1$sex)# PACIENTES DE REF CON DC, DIF EN Presión sistólica RESPECTO AL SEXO  
t.test(train2$sysBP~train2$sex)#sig < PACIENTES DE REF SIN DC, DIF EN Presión sistólica RESPECTO AL SEXO 
t.test(test$sysBP~test$sex)# PACIENTES DE PRUEBA, DIF EN Presión sistólica RESPECTO AL SEXO  

t.test(trainF$sysBP,testF$sysBP)#sig < PACIENTES DE REF CON DC MU vs PACIENTES DE PRUEBA MUJ
t.test(trainM$sysBP,testM$sysBP)#sig < PACIENTES DE REF CON DC H vs PACIENTES DE PRUEBA H

t.test(train$sysBP,test$sysBP)# PACIENTES DE REF vs PACIENTES PRUEBA 
t.test(train1$sysBP,test$sysBP)#sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
t.test(train1$sysBP,train2$sysBP)#sig <  PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
t.test(train2$sysBP,test$sysBP) #no < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA

hist(train$sysBP)
hist(train1$sysBP)
hist(train2$sysBP)
hist(test$sysBP)
hist(trainF$sysBP)
hist(trainM$sysBP)
hist(testF$sysBP)
hist(testM$sysBP)

