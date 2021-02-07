#Paqueterias necesarias para elcodigo
#install.packages("dplyr")
#install.packages("effects")
#install.packages("e1071")
#install.packages("DescTools")
#install.packages("psych")
#install.packages("tidyr")
#install.packages("ResourceSelection")
#install.packages("ggplot2")
library(dplyr)
library(effects)
library(e1071)
library(DescTools)
library(psych)
library(tidyr)
library(ResourceSelection)
library(ggplot2)

# Carga de datos
train <- read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/train_clean.csv")
test<-read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/test_clean.csv")

#AJUSTE DE MODELO -----
attach(train)
str(train)
#Modelo 1 ---- incluye la mayoria de las variables del DF
mod1<-glm(TenYearCHD~age+sex+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+
            totChol+sysBP+diaBP+BMI+heartRate+glucose, family = binomial())
summary(mod1)

#STEP Nos da un mejor modelo considerando un AIC menor (Criterio de información de Akaike)
step(mod1, test="LRT")

#consideramos el último modelo propuesto por el STEP pues tiene el menor AIC
# y el menor número de variables independientes 
mod2<-glm(formula = TenYearCHD ~ age + sex + cigsPerDay + prevalentStroke + 
            totChol + sysBP + glucose, family = binomial())

summary(mod2) #todas las variables son significativas a un 95% de confianza

#pruebas para el modelo-----
#MODELO LOGISTICO 
#Devianza para corroborar si el modelo es adecuado calculamos el 
#cuantil 0.95 de una ji-cuadrada con n-#parametros-1(intercepto) 
n=3390
summary(mod2)

qchisq(0.95,n-8) #Cuantil chi-cuadrada con n-8 grados de libertad

#RD=2545.4<3518.4.6 entonces NO rechazamos Ho,ie,el modelo hace un buen
#ajuste y podriamos decir que los coeficientes considerados no son cero

#medida de Hosmer-Lemeshow 
#H0:El modelo de regresión logística se ajusta a los datos.
#Ha:El modelo de regresión logística no se ajusta a los datos.
hoslem.test(train$TenYearCHD, fitted(mod2))
# como p>005 NO rechazamos Ho, entonces el la regresión se ajusta a los datos

#Pseudo R^2
pseudoR2<-(mod2$null.deviance-deviance(mod2))/mod2$null.deviance
pseudoR2
#este valor nos sirve como refencia para ver que tan bueno
#es el ajuste, este modelo "explica" 11.45% de la variabilidad total
#de nuestros datos originales.
detach(train)

#Los intervalos de confianza
confint(mod2)

#prueba de modelo logistico----
prediccion<-predict(mod2,train,type="response")
train<-mutate(train,prediccion=predict(mod2,train,type="response"))
#Asignamos una probabilidad para asignar uno o cero, donde uno es que la  persona presentara el riesgo y o en caso contrario
for (i  in 1:length(train$sex)) {
  if (is.na(train$prediccion[i])) {train$prediccion[i]=NA}
  else{  if (train$prediccion[i]>=0.55) {train$prediccion[i]=1}
    else {train$prediccion[i]=0}
  }
}

#en cuantos acierta sobre la base de prueba
sum(na.omit(train$TenYearCHD==train$prediccion))

# en cuantos 1 acierta sobre la base de prueba
aciertos1<-filter(train,TenYearCHD==1,prediccion==1)
dim(aciertos1)[1]
#en cuantos ceros acierta sobre la base de prueba
aciertos0<-filter(train,TenYearCHD==0,prediccion==0)
dim(aciertos0)[1]
#cuantos 1 fueron predichos con 0 
errores1<-filter(train,TenYearCHD==1,prediccion==0)
dim(errores1)[1]
#cuantos ceros fueron predichos con 1
errores0<-filter(train,TenYearCHD==0,prediccion==1)
dim(errores0)[1]
#2901  datos  de 3390 fueron predichos correctamente ,es decir hubo un error del 14.42%

#prediccion con modelo logistico ------
#hacemos  la predicción sobre los datos de prueba
prediccion<-predict(mod2,test,type="response")
test<-mutate(test,prediccion=predict(mod2,test,type="response"))

#agregamos la predicción al dataframe
for (i  in 1:length(test$sex)) {
  if (is.na(test$prediccion[i])) {test$prediccion[i]=NA}
  else{  if (test$prediccion[i]>=0.5) {test$prediccion[i]=1}
    else {test$prediccion[i]=0}
  }
} 

#Nos predice que 13 personas resultaran con riesgo 
sum(test$prediccion)
#mujeres con riesgo
mujeres<-filter(test,sex=="1",prediccion==1) #1 mujer
#hombres con riesgo
hombres<-filter(test,sex=="0",prediccion==1)#12 hombres


#escribimos los data frames creados para utilizarlos para la comparacion
write.csv(mujeres,"MujeresRiesgoML.csv")
write.csv(hombres,"HombresRiesgoML.csv")


#Máquina de vectores------
#cargamos de nuevo los datos directo de github para tenerlos limpios
train <- read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/train_clean.csv")
test<-read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/test_clean.csv")

#consideraremos todas las variabels para ver si se logra un mejor ajuste
datos.mv<-select(train,age,sex,cigsPerDay:TenYearCHD)
#convertimos a factor la variable dependiente
datos.mv$TenYearCHD<-factor(datos.mv$TenYearCHD, levels = c("0", "1")) 
str(datos.mv)

#separamos los datos para entrenamiento y prueba
#donde se asignara el 5% de los datos para realizar el entrenamiento y fijamos
#una semilla para poder replicar los resultados
set.seed(2020)
p = sample(nrow(datos.mv), 
           round(nrow(datos.mv)*.75))
entrenamiento<-datos.mv[p,]
prueba<-datos.mv[-p,]

#frecuencias de la variable respuesta
table(entrenamiento$TenYearCHD)
prop.table(table(entrenamiento$TenYearCHD)) %>% round(digits = 2)

#maquina 1------
# Optimización de hiperparámetros mediante validación cruzada 10-fold
#Obtendremos la mejor maquina, pero se comenta pues tarda al cargar
#comentamos esto, pues toma algo de tiempo en encontrar la mejor 
#maquina
#tuning <- tune(svm, TenYearCHD ~ ., data = entrenamiento,
#              kernel="radial")
# Almacenamos el modelo optimo obtenido y accedemos a su información
#modelo <- tuning$best.model
#summary(modelo)

#El mejor modelo obtenido sería equivalente a ajustar:
modelo<- svm(TenYearCHD ~ ., data = entrenamiento, 
             kernel = "radial", 
             cost = 1)

#predicciones con base de entrenamiento
predicciones = predict(modelo, entrenamiento)
table(prediccion = predicciones, real = entrenamiento$TenYearCHD)
#obtenemos el porcentaje de error con los datos de entrenamiento 
paste("Observaciones de test mal clasificadas:", 
      100 * mean(entrenamiento$TenYearCHD != predict(modelo,entrenamiento)) %>%
        round(digits = 4), "%") #error del 13.61%

# Error con base de prueba
predicciones = predict(modelo, prueba)
table(prediccion = predicciones, real = prueba$TenYearCHD)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(prueba$TenYearCHD != predict(modelo, prueba)) %>%
        round(digits = 4), "%") #error de 14.98%

#prediccion de datos 
predicciones = predict(modelo, test)
a<-as.data.frame(predicciones)
a$predicciones<-as.numeric(as.character(a$predicciones))
sum(a) #solo clasifico a 4 con riesgo por lo que intentaremos considerar los factores que resultaron 
#significativos en el modelo logistico


#segundo intento con las variables del modelo logistico------
#age + sex + cigsPerDay + prevalentStroke + totChol + sysBP + glucose
#cargamos de nuevo los datos para tenerlos limpios
train <- read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/train_clean.csv")
test<-read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/test_clean.csv")

#seleccionamos los factores que incluiremos en el modelo
#basandonos en los análisis previos y en el modelo logistico donde
#esta variables resultadon relevantes
datos.mv<-select(train,age,sex,cigsPerDay,prevalentStroke,totChol,sysBP,glucose,TenYearCHD)
#convertimos a factor la variable dependiente
datos.mv$TenYearCHD<-factor(datos.mv$TenYearCHD, levels = c("0", "1")) 
str(datos.mv)

#separamos los datos para entrenamiento y prueba
set.seed(2020)
p = sample(nrow(datos.mv), 
           round(nrow(datos.mv)*.75))
entrenamiento<-datos.mv[p,]
prueba<-datos.mv[-p,]

#frecuencias de la variable respuesta
table(entrenamiento$TenYearCHD)
prop.table(table(entrenamiento$TenYearCHD)) %>% round(digits = 2)

#maquina 2------------ 
# Optimización de hiperparámetros mediante validación cruzada 10-fold
#se comenta pues toma tiempo en lograr encontrar la mejor maquina

# tuning <- tune(svm, TenYearCHD ~ ., data = entrenamiento, 
#                kernel = "radial", 
#                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20)))

#summary(tuning)
# Almacenamos el modelo optimo obtenido y accedemos a su información
#modelo <- tuning$best.model
#summary(modelo)
#El proceso de cross-validation muestra que el valor de penalización con el que se consigue menor 
#error rate es 5.

#El mejor modelo obtenido sería equivalente a ajustar:
modelo<- svm(TenYearCHD ~ ., data = entrenamiento, 
             kernel = "radial", 
             cost = 5)

#predicciones con datos de entrenamiento
predicciones = predict(modelo, entrenamiento)
table(prediccion = predicciones, real = entrenamiento$TenYearCHD)
#13.297% de los datos de entrenamiento fueron mal clasificados
paste("Observaciones de test mal clasificadas:", 
      100 * mean(entrenamiento$TenYearCHD != predict(modelo, entrenamiento)) %>%
        round(digits = 5), "%")

# Error de test
predicciones = predict(modelo, prueba)
table(prediccion = predicciones, real = prueba$TenYearCHD)
#14.62 de los datos de pruebas fueron mal clasificados
paste("Observaciones de test mal clasificadas:", 
      100 * mean(prueba$TenYearCHD != predict(modelo, prueba)) %>%
        round(digits = 4), "%")

#prediccion de datos 
predicciones = predict(modelo, test)
a<-as.data.frame(predicciones)
a$predicciones<-as.numeric(as.character(a$predicciones))
#obtenemos que 13 personas tienen riesgo de sufrir alguna enfermedad en un periodo de 10 años
sum(a)
test$TenYearCHD<-a$predicciones

#hombres y mujeres con riesgo  predicciones
mujeres.riesgo<-filter(test,TenYearCHD==1,sex=="1")#1 mujer con riesgo
hombres.riesgo<-filter(test,TenYearCHD==1,sex=="0")#11 hombres con riesgo

#guardamos los data frame con formato csv
write.csv(mujeres.riesgo,"MujeresRiesgoMV.csv")
write.csv(hombres.riesgo,"HombresriesgoMV.csv")


#Data frames de predicciones para Shiny
mujeresml<-read.csv("MujeresRiesgoML.csv")
mujeresmv<-read.csv("MujeresRiesgoMV.csv")
hombresml<-read.csv("HombresRiesgoML.csv")
hombresmv<-read.csv("HombresRiesgoMV.csv")
#Seleccionamos las variables de interes
mujeresml<-select(mujeresml,id,age,cigsPerDay,prevalentStroke,totChol,sysBP,glucose)
mujeresmv<-select(mujeresmv,id,age,cigsPerDay,prevalentStroke,totChol,sysBP,glucose)
hombresml<-select(hombresml,id,age,cigsPerDay,prevalentStroke,totChol,sysBP,glucose)
hombresmv<-select(hombresmv,id,age,cigsPerDay,prevalentStroke,totChol,sysBP,glucose)
#Renombramos las columnas
mujeresml<-rename(mujeresml,"ID"=id,"Edad"=age,"Cigarros_al_dia"=cigsPerDay,"Ataque_cardiaco_previo"=prevalentStroke,"Nivel_de_colesterol"=totChol,
                  "Presion_arterial_sistolica"=sysBP,"Nivel_glucosa"=glucose)
mujeresmv<-rename(mujeresmv,"ID"=id,"Edad"=age,"Cigarros_al_dia"=cigsPerDay,"Ataque_cardiaco_previo"=prevalentStroke,"Nivel_de_colesterol"=totChol,
                  "Presion_arterial_sistolica"=sysBP,"Nivel_glucosa"=glucose)
hombresml<-rename(hombresml,"ID"=id,"Edad"=age,"Cigarros_al_dia"=cigsPerDay,"Ataque_cardiaco_previo"=prevalentStroke,"Nivel_de_colesterol"=totChol,
                  "Presion_arterial_sistolica"=sysBP,"Nivel_glucosa"=glucose)
hombresmv<-rename(hombresmv,"ID"=id,"Edad"=age,"Cigarros_al_dia"=cigsPerDay,"Ataque_cardiaco_previo"=prevalentStroke,"Nivel_de_colesterol"=totChol,
                  "Presion_arterial_sistolica"=sysBP,"Nivel_glucosa"=glucose)

#guardamos las bases limpias para presentarlas en el dashboard
write.csv(mujeresml,"Mujeres_modelo_logistico.csv")
write.csv(mujeresmv,"Mujeres_maquina_vectores.csv")
write.csv(hombresml,"Hombres_modelo_logistico.csv")
write.csv(hombresmv,"Hombres_maquina_vectores.csv")

