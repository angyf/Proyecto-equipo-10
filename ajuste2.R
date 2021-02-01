library(dplyr)
library(effects)
library(e1071)#para impute
library(DescTools) #para la moda

test<-read.csv("proyecto/test.csv")
train<-read.csv("proyecto/train.csv")
#convertir la var sexo a numerico
train<-mutate(train,sex=as.character(sex))

#llenamos los NA de las variables que tomaremos como independientes con la media de
# la variable respectiva 

#EDAD,SEXO,NO TIENEN NULOS
sum(is.na(train$age))
sum(is.na(train$sex))
#BPMeds,revalentStroke,PrevalentHyp,Diabetes NO PUEDEN COMPLETARSE PUES SON NOMINALES
#las completamos con la moda ???????????????

Mode(na.omit(train$BPMeds)) #la moda es 0
train$BPMeds<-ifelse(is.na(train$BPMeds),0,train$BPMeds)

Mode(na.omit(train$prevalentStroke)) #la moda es 0
train$prevalentStroke<-ifelse(is.na(train$prevalentStroke),0,train$prevalentStroke)

Mode(na.omit(train$prevalentHyp)) #la moda es 0
train$prevalentHyp<-ifelse(is.na(train$prevalentHyp),0,train$prevalentHyp)

Mode(na.omit(train$diabetes)) #la moda es 0
train$diabetes<-ifelse(is.na(train$diabetes),0,train$diabetes)

#CigsPerDay totChol+sysBP+diaBP+BMI+heartRate+glucose
train<-mutate(train,cigsPerDay=as.numeric(impute(as.data.frame(train$cigsPerDay))))
train<-mutate(train,totChol=as.numeric(impute(as.data.frame(train$totChol))))
train<-mutate(train,sysBP=as.numeric(impute(as.data.frame(train$sysBP))))
train<-mutate(train,diaBP=as.numeric(impute(as.data.frame(train$diaBP))))
train<-mutate(train,BMI=as.numeric(impute(as.data.frame(train$BMI))))
train<-mutate(train,heartRate=as.numeric(impute(as.data.frame(train$heartRate))))
train<-mutate(train,glucose=as.numeric(impute(as.data.frame(train$glucose))))


#convertimos la variable categorica a numerica
#0 Hombre, 1 mujer
for (i  in 1:length(train$sex)) {
  if ((train$sex[i])=="M") {train$sex[i]=0}
  else {train$sex[i]=1}
}

#dividir los datos de entrenamiento

#attach(train)
attach(train)
#Modelo 1 
mol1<-glm(TenYearCHD~age+sex+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+
            totChol+sysBP+diaBP+BMI+heartRate+glucose, family = binomial())
summary(mol1)
#Nos daba un mejor modelo considerando un AIC menor (Criterio de información de Akaike)
step(mol1, test="LRT")

#modelo 2
mod2<-glm(TenYearCHD~age + sex + cigsPerDay + prevalentStroke + totChol+sysBP +glucose,
          family=binomial())
summary(mod2)


##########################

prediccion<-predict(mod2,train,type="response")
sum(na.exclude(prediccion>1))
train<-mutate(train,prediccion=predict(mod2,train,type="response"))

for (i  in 1:length(train$sex)) {
  if (is.na(train$prediccion[i])) {train$prediccion[i]=NA}
  else{  if (train$prediccion[i]>=0.6) {train$prediccion[i]=1}
    else {train$prediccion[i]=0}
  }
}
#en cuantos acierta
sum(na.omit(train$TenYearCHD==train$prediccion))

# en cuantos 1 acierta
aciertos1<-filter(train,TenYearCHD==1,prediccion==1)
dim(aciertos1)[1]
#en cuantos ceros acierta
aciertos0<-filter(train,TenYearCHD==0,prediccion==0)
dim(aciertos0)[1]
#cuantos 1 fueron predichos con 0
errores1<-filter(train,TenYearCHD==1,prediccion==0)
dim(errores1)[1]
#cuantos ceros fueron predichos con 1
errores0<-filter(train,TenYearCHD==0,prediccion==1)
dim(errores0)[1]


detach(train)
