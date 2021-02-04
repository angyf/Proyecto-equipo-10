library(e1071)#para maquina de vectores
library(dplyr)

train<-read.csv("train.csv")
pruebas<-read.csv("train.csv")
#Limpieza de datos
#completamos las variables categoricas con la moda
Mode(na.omit(train$BPMeds)) #la moda es 0
train$BPMeds<-ifelse(is.na(train$BPMeds),0,train$BPMeds)
Mode(na.omit(train$prevalentStroke)) #la moda es 0
train$prevalentStroke<-ifelse(is.na(train$prevalentStroke),0,train$prevalentStroke)
Mode(na.omit(train$prevalentHyp)) #la moda es 0
train$prevalentHyp<-ifelse(is.na(train$prevalentHyp),0,train$prevalentHyp)
Mode(na.omit(train$diabetes)) #la moda es 0
train$diabetes<-ifelse(is.na(train$diabetes),0,train$diabetes)

#Cambiamos NA con la mediana de los datos, para un mejor manejo
train<-mutate(train,cigsPerDay=as.numeric(impute(as.data.frame(train$cigsPerDay),what="median")))
train<-mutate(train,totChol=as.numeric(impute(as.data.frame(train$totChol),what="median")))
train<-mutate(train,sysBP=as.numeric(impute(as.data.frame(train$sysBP),what="median")))
train<-mutate(train,diaBP=as.numeric(impute(as.data.frame(train$diaBP),what="median")))
train<-mutate(train,BMI=as.numeric(impute(as.data.frame(train$BMI),what="median")))
train<-mutate(train,heartRate=as.numeric(impute(as.data.frame(train$heartRate),what="median")))
train<-mutate(train,glucose=as.numeric(impute(as.data.frame(train$glucose),what="median")))


#Conversion de datos para modelos -----
#convertimos la variable categorica a numerica
#0 Hombre, 1 mujer
train<-mutate(train,sex=as.character(train$sex))
for (i  in 1:length(train$sex)) {
  if (train$sex[i]=="F") {train$sex[i]=1}
  else {train$sex[i]=0}
}


datos.mv<-select(train,age,sex,cigsPerDay:TenYearCHD)
#convertimos a factor la variable dependiente
datos.mv$TenYearCHD<-factor(datos.mv$TenYearCHD, levels = c("0", "1")) 
str(datos.mv)

#separamos los datos para entrenamiento y prueba
#ind<-sample(2,nrow(datos.mv),replace=TRUE,prob=c(0.7,.3))
#3390*.75 =2542.5
p = sample(nrow(datos.mv), 
           round(nrow(datos.mv)/2))
entrenamiento<-datos.mv[p,]
prueba<-datos.mv[-p,]

#frecuencias de la variable respuesta
table(entrenamiento$TenYearCHD)
prop.table(table(entrenamiento$TenYearCHD)) %>% round(digits = 2)


# Optimización de hiperparámetros mediante validación cruzada 10-fold
set.seed(325)
tuning <- tune(svm, TenYearCHD ~ ., data = entrenamiento, 
               kernel = "radial", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20)), 
               scale = TRUE)

summary(tuning)
# Almacenamos el modelo optimo obtenido y accedemos a su información
modelo <- tuning$best.model
summary(modelo)


#El mejor modelo obtenido sería equivalente a ajustar:
 modelo<- svm(TenYearCHD ~ ., data = entrenamiento, 
                    kernel = "radial", 
                    cost = 1, 
                    scale = TRUE) #no clasifica ningun 1-1 ni 1-0

 # Error de test
 predicciones = predict(modelo, prueba)
 table(prediccion = predicciones, real = prueba$TenYearCHD)
 
 paste("Observaciones de test mal clasificadas:", 
       100 * mean(prueba$TenYearCHD != predict(modelo, prueba)) %>%
         round(digits = 4), "%")
 
 
#segundo intento con las variables del modelo logistico------
#age + sex + cigsPerDay + prevalentStroke + totChol + sysBP + glucose
 
 datos.mv<-select(train,age,sex,cigsPerDay,prevalentStroke,totChol,sysBP,glucose,TenYearCHD)
 #convertimos a factor la variable dependiente
 datos.mv$TenYearCHD<-factor(datos.mv$TenYearCHD, levels = c("0", "1")) 
 str(datos.mv)
 
 #separamos los datos para entrenamiento y prueba
 #ind<-sample(2,nrow(datos.mv),replace=TRUE,prob=c(0.7,.3))
 #3390*.75 =2542.5
 p = sample(nrow(datos.mv), 
                round(nrow(datos.mv)/2))
 entrenamiento<-datos.mv[p,]
 prueba<-datos.mv[-p,]
 
  #frecuencias de la variable respuesta
 table(entrenamiento$TenYearCHD)
 prop.table(table(entrenamiento$TenYearCHD)) %>% round(digits = 2)
 
 
 # Optimización de hiperparámetros mediante validación cruzada 10-fold
 set.seed(325)
 tuning <- tune(svm, TenYearCHD ~ ., data = entrenamiento, 
                kernel = "radial", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20)), 
                scale = TRUE)
 
 summary(tuning)
 # Almacenamos el modelo optimo obtenido y accedemos a su información
 modelo <- tuning$best.model
 summary(modelo)
 
 
 #El mejor modelo obtenido sería equivalente a ajustar:
 modelo<- svm(TenYearCHD ~ ., data = entrenamiento, 
              kernel = "radial", 
              cost = 20, 
              scale = TRUE) #no clasifica ningun 1-1 ni 1-0
 
 # Error de test
 predicciones = predict(modelo, prueba)
 table(prediccion = predicciones, real = prueba$TenYearCHD)
 
 paste("Observaciones de test mal clasificadas:", 
       100 * mean(prueba$TenYearCHD != predict(modelo, prueba)) %>%
         round(digits = 4), "%")
 
 
#con el archivo test<-pruebas
pre<-predict(modelo, pruebas)
#total de personas con riesgo 
sum(as.numeric(as.character(pre))) #41
#porcentaje con riesgo 1.20
100*sum(as.numeric(as.character(pre)))/nrow(pruebas)

#porcentaje con riesgo en train
100*sum(train$TenYearCHD)/nrow(train)
 