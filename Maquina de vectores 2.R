library(dplyr)
library(effects)
library(e1071)#para impute
library(DescTools) #para la moda
library(psych)
library(tidyr)
library(ResourceSelection)
library(ggplot2)


# Carga de datos
setwd("proyecto/")
train <- read.csv("train.csv")
test<-read.csv("test.csv")

# Analisis general de los datos-----

head( train )

str( train )

dim( train )

View( train )

#Carga y limpieza de datos-----
# Verificar el numero de datos marcados como NA - 

#EDAD,SEXO,NO TIENEN NULOS
sum(is.na(train$age))
sum(is.na(train$sex))
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

#Carga y limpieza de datos de prueba----
Mode(na.omit(test$BPMeds)) #la moda es 0
test$BPMeds<-ifelse(is.na(test$BPMeds),0,test$BPMeds)

Mode(na.omit(test$education)) #la moda es 0
test$education<-ifelse(is.na(test$education),0,test$education)

Mode(na.omit(test$prevalentStroke)) #la moda es 0
test$prevalentStroke<-ifelse(is.na(test$prevalentStroke),0,test$prevalentStroke)

Mode(na.omit(test$prevalentHyp)) #la moda es 0
test$prevalentHyp<-ifelse(is.na(test$prevalentHyp),0,test$prevalentHyp)

Mode(na.omit(test$diabetes)) #la moda es 0
test$diabetes<-ifelse(is.na(test$diabetes),0,test$diabetes)

#Cambiamos NA con la mediana de los datos, para un mejor manejo
test<-mutate(test,cigsPerDay=as.numeric(impute(as.data.frame(test$cigsPerDay),what="median")))
test<-mutate(test,totChol=as.numeric(impute(as.data.frame(test$totChol),what="median")))
test<-mutate(test,sysBP=as.numeric(impute(as.data.frame(test$sysBP),what="median")))
test<-mutate(test,diaBP=as.numeric(impute(as.data.frame(test$diaBP),what="median")))
test<-mutate(test,BMI=as.numeric(impute(as.data.frame(test$BMI),what="median")))
test<-mutate(test,heartRate=as.numeric(impute(as.data.frame(test$heartRate),what="median")))
test<-mutate(test,glucose=as.numeric(impute(as.data.frame(test$glucose),what="median")))

#filtros-----

fumadores_diabeticos <- filter( train, is_smoking == 'YES' & diabetes == 1  ) # 29 Observaciones
women_fumadores_diabeticos <- filter( train, is_smoking == 'YES' & sex == 'F' ) # 16 Observaciones
men_fumadores_diabeticos <- filter( train, is_smoking == 'YES' & sex == 'M' ) # 13 Observaciones

fumadores_no_diabeticos <- filter( train, is_smoking == 'YES' & diabetes == 0 ) # 29 Observaciones
women_fumadores_no_diabeticos <- filter( train, is_smoking == 'YES' & sex == 'F' ) # 16 Observaciones
men_fumadores_no_diabeticos <- filter( train, is_smoking == 'YES' & sex == 'M' ) # 13 Observaciones


# Por rangos de edades en 10
age_30_40  <- filter( train, age > 29 & age < 41 ) # 604 Observaciones
age_40_50  <- filter( train, age > 39 & age < 51 ) # 1431 Observaciones
age_50_60  <- filter( train, age > 49 & age < 61 ) # 1156 Observaciones
age_60_70  <- filter( train, age > 59 & age < 71 ) # 557 Obervaciones
age_70_80  <- filter( train, age > 69 & age < 81 ) # 2 Obervaciones


# Por rangos de edades en 10 y fumadores
smoking_age_30_40  <- filter( age_30_40, is_smoking == 'YES' ) # 369 Observaciones
smoking_age_40_50  <- filter( age_40_50, is_smoking == 'YES' ) # 834 Observaciones
smoking_age_50_60  <- filter( age_50_60, is_smoking == 'YES' ) # 497 Observaciones *
smoking_age_60_70  <- filter( age_60_70, is_smoking == 'YES' ) # 175 Observaciones *
smoking_age_70_80  <- filter( age_70_80, is_smoking == 'YES' ) # 0 Observaciones   *

# Por rangos de edades en 10 y no fumadores
no_smoking_age_30_40  <- filter( age_30_40, is_smoking == 'NO' ) # 235 Observaciones
no_smoking_age_40_50  <- filter( age_40_50, is_smoking == 'NO' ) # 597 Observaciones
no_smoking_age_50_60  <- filter( age_50_60, is_smoking == 'NO' ) # 659 Observaciones *
no_smoking_age_60_70  <- filter( age_60_70, is_smoking == 'NO' ) # 382 Observaciones *
no_smoking_age_70_80  <- filter( age_70_80, is_smoking == 'NO' ) # 2 Observaciones   *


# Por rangos de edades en 10, fumadores y con diabetes
diabetes_smoking_age_30_40  <- filter( smoking_age_30_40, diabetes == 1 ) # 1 Observaciones
diabetes_smoking_age_40_50  <- filter( smoking_age_40_50, diabetes == 1 ) # 13 Observaciones
diabetes_smoking_age_50_60  <- filter( smoking_age_50_60, diabetes == 1 ) # 12 Observaciones 
diabetes_smoking_age_60_70  <- filter( smoking_age_60_70, diabetes == 1 ) # 6 Observaciones 
diabetes_smoking_age_70_80  <- filter( smoking_age_70_80, diabetes == 1 ) # 0 Observaciones   


# Por rangos de edades en 10, no fumadores y con diabetes
diabetes_no_smoking_age_30_40  <- filter( no_smoking_age_30_40, diabetes == 1 ) # 2 Observaciones *
diabetes_no_smoking_age_40_50  <- filter( no_smoking_age_40_50, diabetes == 1 ) # 8 Observaciones
diabetes_no_smoking_age_50_60  <- filter( no_smoking_age_50_60, diabetes == 1 ) # 27 Observaciones *
diabetes_no_smoking_age_60_70  <- filter( no_smoking_age_60_70, diabetes == 1 ) # 24 Observaciones *
diabetes_no_smoking_age_70_80  <- filter( no_smoking_age_70_80, diabetes == 1 ) # 0 Observaciones  


# Por genero edad, con diabates y fumadores
women_diabetes_smoking_age_30_40  <- filter( diabetes_smoking_age_30_40, sex == 'F' ) # 0 Observaciones
women_diabetes_smoking_age_40_50  <- filter( diabetes_smoking_age_40_50, sex == 'F' ) # 8 Observaciones
women_diabetes_smoking_age_50_60  <- filter( diabetes_smoking_age_50_60, sex == 'F' ) # 8 Observaciones
women_diabetes_smoking_age_60_70  <- filter( diabetes_smoking_age_60_70, sex == 'F' ) # 3 Obervaciones
women_diabetes_smoking_age_70_80  <- filter( diabetes_smoking_age_70_80, sex == 'F' ) # 2 Obervaciones

# Por genero edad, con diabates y fumadores
men_diabetes_smoking_age_30_40  <- filter( diabetes_smoking_age_30_40, sex == 'M' ) # 1 Observaciones
men_diabetes_smoking_age_40_50  <- filter( diabetes_smoking_age_40_50, sex == 'M' ) # 5 Observaciones
men_diabetes_smoking_age_50_60  <- filter( diabetes_smoking_age_50_60, sex == 'M' ) # 4 Observaciones
men_diabetes_smoking_age_60_70  <- filter( diabetes_smoking_age_60_70, sex == 'M' ) # 3 Obervaciones
men_diabetes_smoking_age_70_80  <- filter( diabetes_smoking_age_70_80, sex == 'M' ) # 0 Obervaciones

# -----------------------------------------------------------------------------------------

# Edad contra BMI - Indicie de masa muscular
ggplot( train, aes( x = age, y = BMI )) + 
  geom_point() +  
  xlab('Edad') +  
  ylab('Indice de masa corporal')

# Fumadores diabeticos, edad y cigarrillos al dia
ggplot( fumadores_diabeticos, aes( x = age, y = cigsPerDay ) ) + 
  geom_point() +  
  xlab('Edad') +  
  ylab('Cigarros al dia')

# Fumadores NO diabeticos, edad y cigarrillos al dia
ggplot( fumadores_no_diabeticos, aes( x = age, y = cigsPerDay )) + 
  geom_point() +
  xlab('Edad') +  
  ylab('Cigarros al dia')

names(fumadores_no_diabeticos)

summary( train )

hist(train$age, 
     breaks = 15, 
     main = "Histograma Edades",
     xlab = "Rango Edades",
     ylab = "Frecuencia",xlim=c(30,70),col=brewer.pal(8, "Set2"))

# train %>%
#   ggplot() + 
#   aes(train$age) +
#   geom_histogram(binwidth = 10, col="black", fill = "blue") + 
#   ggtitle("Histograma de Edades") +
#   ylab("Frecuencia") +
#   xlab("Edades") + 
#   theme_light()


# ggplot(train, aes(x = age, y = cigsPerDay, fill = diabetes)) +
#   geom_boxplot() +
#   ggtitle("Boxplots") +
#   xlab("Edad") +
#   ylab("Cigarros al dia")

cardio <- rename(train, 
                 id = id, 
                 edad = age,
                 educacion = education,
                 genero = sex,
                 fumador = is_smoking,
                 cigarrosAlDia = cigsPerDay,
                 BPMeds = BPMeds,
                 accidenteCerebrovascularPrevalente = prevalentStroke,
                 hipertenionPrevalente = prevalentHyp,
                 diabetes = diabetes,
                 totalColasterol = totChol,
                 sysBP = sysBP,
                 diaBP = diaBP,
                 indiceMasaMuscular= BMI,
                 ritmoCardiaco = heartRate,
                 glucosa = glucose,
                 enfermedad10CHD = TenYearCHD)

#GRAFICOS----
#Cuantos hombres y cuantas mujeres tienen TenYearsCHD
colores<-c("pink","red","aquamarine","blue")
levels(train$sex) <- c("Mujer", "Hombre")
ggplot(train,aes(x=factor(TenYearCHD)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train$sex)+
  labs(x="",y="Total",
       title="Personas con riesgo de enfermedad en 10 años")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('Sin riesgo','Con riesgo'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

train1<-filter(train,TenYearCHD==1)
#boxplot de presion arteria por sexo 
colores.box<-c("pink","lightblue")
ggplot(train1,aes(sex,heartRate),y=heartRate)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Frecuencia cardiaca")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para colesterol
ggplot(train1,aes(sex,totChol),y=totChol)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Nivel de colesterol")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para glucosa por sexo
ggplot(train1,aes(sex,glucose),y=glucose)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Nivel de glucosa")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para presión sistolica por sexo
ggplot(train1,aes(sex,sysBP),y=sysBP)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Presión sistolica")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#Boxplot para glucosa por sexo
ggplot(train1,aes(sex,diaBP),y=diaBP)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Presion diastolica")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Frecuencias de diabetes
ggplot(train1,aes(x=factor(diabetes)))+
  geom_bar(color = "black", fill ="lightblue",aes(y = (..count..)/sum(..count..)))+
  labs(x="",y="Total",
       title="Frecuencia de diabetes")+
  scale_x_discrete(labels = c('Sin diabetes','Con diabetes'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent_format(percent=100,scale = 100))


#Conversion de datos para modelos -----
#convertimos la variable categorica a numerica
#0 Hombre, 1 mujer
train<-mutate(train,sex=as.character(train$sex))
for (i  in 1:length(train$sex)) {
  if (train$sex[i]=="F") {train$sex[i]=1}
  else {train$sex[i]=0}
}

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
summary(mod2) #todas las variables son significativas

#pruebas para el modelo-----
#MODELO 1
#Devianza para corroborar si el modelo es adecuado calculamos el 
#cuantil 0.95 de una ji-cuadrada con n-#parametros-1(intercepto) 
n=2927
summary(mod2)

qchisq(0.95,n-8)

#RD=2545.4<3040.6 entonces NO rechazamos Ho,ie,el modelo hace un buen
#ajuste y podriamos decir que los coeficientes considerados no son cero

#medida de Hosmer-Lemeshow 
#H0:El modelo de regresión logística se ajusta a los datos.
#Ha:El modelo de regresión logística no se ajusta a los datos.
hoslem.test(train$TenYearCHD, fitted(mod2))
# como p>005 NO rechazamos Ho

#Pseudo R^2
pseudoR2<-(mod2$null.deviance-deviance(mod2))/mod2$null.deviance
pseudoR2
#este valor nos sirve como refencia para ver que tan bueno
#es el ajuste, este modelo "explica" 11.45% de la variabilidad total
#de nuestros datos originales.
detach(train)

#Exponencial de los coeficientes, lo que se interpreta
exp(coefficients(mod2))

#Los intervalos de confianza
confint(mod2)

#PREDICCION----

prediccion<-predict(mod2,train,type="response")
#sum(na.exclude(prediccion>1))
train<-mutate(train,prediccion=predict(mod2,train,type="response"))

for (i  in 1:length(train$sex)) {
  if (is.na(train$prediccion[i])) {train$prediccion[i]=NA}
  else{  if (train$prediccion[i]>=0.55) {train$prediccion[i]=1}
    else {train$prediccion[i]=0}
  }
}

#Prueba-----
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


#Maquina de vectores----
library(e1071)#para maquina de vectores
library(dplyr)

train<-read.csv("train.csv")
pruebas<-read.csv("test.csv")
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

Mode(na.omit(test$BPMeds)) #la moda es 0
test$BPMeds<-ifelse(is.na(test$BPMeds),0,test$BPMeds)

Mode(na.omit(test$education)) #la moda es 0
test$education<-ifelse(is.na(test$education),0,test$education)

Mode(na.omit(test$prevalentStroke)) #la moda es 0
test$prevalentStroke<-ifelse(is.na(test$prevalentStroke),0,test$prevalentStroke)

Mode(na.omit(test$prevalentHyp)) #la moda es 0
test$prevalentHyp<-ifelse(is.na(test$prevalentHyp),0,test$prevalentHyp)

Mode(na.omit(test$diabetes)) #la moda es 0
test$diabetes<-ifelse(is.na(test$diabetes),0,test$diabetes)

#Cambiamos NA con la mediana de los datos, para un mejor manejo
test<-mutate(test,cigsPerDay=as.numeric(impute(as.data.frame(test$cigsPerDay),what="median")))
test<-mutate(test,totChol=as.numeric(impute(as.data.frame(test$totChol),what="median")))
test<-mutate(test,sysBP=as.numeric(impute(as.data.frame(test$sysBP),what="median")))
test<-mutate(test,diaBP=as.numeric(impute(as.data.frame(test$diaBP),what="median")))
test<-mutate(test,BMI=as.numeric(impute(as.data.frame(test$BMI),what="median")))
test<-mutate(test,heartRate=as.numeric(impute(as.data.frame(test$heartRate),what="median")))
test<-mutate(test,glucose=as.numeric(impute(as.data.frame(test$glucose),what="median")))

#Conversion de datos para modelos -----
#convertimos la variable categorica a numerica
#0 Hombre, 1 mujer
train<-mutate(train,sex=as.character(train$sex))
for (i  in 1:length(train$sex)) {
  if (train$sex[i]=="F") {train$sex[i]=1}
  else {train$sex[i]=0}
}

test<-mutate(test,sex=as.character(test$sex))
for (i  in 1:length(test$sex)) {
  if (test$sex[i]=="F") {test$sex[i]=1}
  else {test$sex[i]=0}
}

datos.mv<-select(train,age,sex,cigsPerDay:TenYearCHD)
#convertimos a factor la variable dependiente
datos.mv$TenYearCHD<-factor(datos.mv$TenYearCHD, levels = c("0", "1")) 
str(datos.mv)

#separamos los datos para entrenamiento y prueba
#ind<-sample(2,nrow(datos.mv),replace=TRUE,prob=c(0.7,.3))
#3390*.75 =2542.5
set.seed(1998)
p = sample(nrow(datos.mv), 
           round(nrow(datos.mv)/2))
entrenamiento<-datos.mv[p,]
prueba<-datos.mv[-p,]

#frecuencias de la variable respuesta
table(entrenamiento$TenYearCHD)
prop.table(table(entrenamiento$TenYearCHD)) %>% round(digits = 2)

entrenamiento$TenYearCHD<-factor(entrenamiento$TenYearCHD) 
test<-mutate(test,sex=as.numeric(test$sex))

# Optimización de hiperparámetros mediante validación cruzada 10-fold

tuning <- tune(svm, TenYearCHD ~ ., data = entrenamiento)

summary(tuning)
# Almacenamos el modelo optimo obtenido y accedemos a su información
modelo <- tuning$best.model
summary(modelo)


#El mejor modelo obtenido sería equivalente a ajustar:
modelo<- svm(TenYearCHD ~ ., data = entrenamiento, 
             kernel = "radial", 
             cost = 1) 

# Error de test
predicciones = predict(modelo, prueba)
table(prediccion = predicciones, real = prueba$TenYearCHD)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(prueba$TenYearCHD != predict(modelo, prueba)) %>%
        round(digits = 4), "%")

predicciones = predict(modelo, entrenamiento)
table(prediccion = predicciones, real = entrenamiento$TenYearCHD)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(entrenamiento$TenYearCHD != predict(modelo, entrenamiento)) %>%
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
set.seed(1998)
p = sample(nrow(datos.mv), 
           round(nrow(datos.mv)/2))
entrenamiento<-datos.mv[p,]
prueba<-datos.mv[-p,]

#frecuencias de la variable respuesta
table(entrenamiento$TenYearCHD)
prop.table(table(entrenamiento$TenYearCHD)) %>% round(digits = 2)


# Optimización de hiperparámetros mediante validación cruzada 10-fold

tuning <- tune(svm, TenYearCHD ~ ., data = entrenamiento)

summary(tuning)
# Almacenamos el modelo optimo obtenido y accedemos a su información
modelo <- tuning$best.model
summary(modelo)


#El mejor modelo obtenido sería equivalente a ajustar:
modelo<- svm(TenYearCHD ~ ., data = entrenamiento, 
             cost = 1)

# Error de test
predicciones = predict(modelo, prueba)
table(prediccion = predicciones, real = prueba$TenYearCHD)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(prueba$TenYearCHD != predict(modelo, prueba)) %>%
        round(digits = 4), "%")

#Prueba con datos de entrenamiento 
predicciones = predict(modelo, entrenamiento)
table(prediccion = predicciones, real = entrenamiento$TenYearCHD)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(entrenamiento$TenYearCHD != predict(modelo, entrenamiento)) %>%
        round(digits = 4), "%")



#con el archivo test<-pruebas
test1<-select(test,age, sex,cigsPerDay,prevalentStroke,
              totChol, sysBP, glucose)
pre<-predict(modelo, pruebas)
#total de personas con riesgo 
sum(as.numeric(as.character(pre))) #41
#porcentaje con riesgo 1.20
100*sum(as.numeric(as.character(pre)))/nrow(pruebas)

#porcentaje con riesgo en train
100*sum(train$TenYearCHD)/nrow(train)

test$TenYearCHD<-as.numeric(as.character(pre))


#GRAFICOS PREDICCION
colores<-c("pink","red","aquamarine","blue")
levels(test$sex) <- c("Mujer", "Hombre")
ggplot(test,aes(x=factor(TenYearCHD)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(test$sex)+
  labs(x="",y="Total",
       title="Personas con riesgo de enfermedad en 10 años")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('Sin riesgo','Con riesgo'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


