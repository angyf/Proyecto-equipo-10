library(dplyr)
library(effects)
library(e1071)#para impute
library(DescTools) #para la moda
library(psych)
library(tidyr)
library(ResourceSelection)
library(ggplot2)


#La Organización Mundial de la Salud ha estimado que ocurren 12 millones de muertes en todo el mundo, 
#cada año debido a enfermedades cardíacas. La mitad de las muertes en los Estados Unidos y otros países
#desarrollados se deben a enfermedades cardiovasculares. El pronóstico temprano de las enfermedades 
#cardiovasculares puede ayudar a tomar decisiones sobre cambios en el estilo de vida en pacientes de 
#alto riesgo y, a su vez, reducir las complicaciones.

#Es por ello que en este análisis nos enfocamos en determinar qué factores podrían estar implicados de
#manera importante en el desarrollo de la enfermedad coronaria crónica, y usarlos para predecir qué pacienes
#son más propensos a tener el riesgo de enfermedad.

# Carga de datos----
setwd("/Users/...")

train <- read.csv("train.csv") #Base de datos con la población de referencia, es decir, 
#la población a quien ya se determinó si sufrirán o no la enfermedad coronario crónica a diez años (TenYearCHD)
#la cual será la variable dependiente (x)

test<-read.csv("test.csv") ##Base de datos con la población de prueba, es decir, 
#la población a quien NO se le ha determinado si sufrirán o no la enfermedad coronario crónica a diez años

# Analisis general de los datos-----

#Analizar tipo de datos, dimensiones de las BD, estructura, y resúmenes estadísticos de las variables


head( train )

str( train )

dim( train )

View( train )

head( test )

str( test )

dim( test )

View( test )

summary(train) 

summary(test)

#Carga y limpieza de datos-----

# Verificar el numero de datos marcados como NA - 

#EDAD,SEXO,NO TIENEN NULOS
sum(is.na(train$age))
sum(is.na(train$sex))

sum(is.na(test$age))
sum(is.na(test$sex))

#completamos las variables categoricas con la moda

#TRAIN (poblacion de referencia)
Mode(na.omit(train$BPMeds)) #la moda es 0
train$BPMeds<-ifelse(is.na(train$BPMeds),0,train$BPMeds)

Mode(na.omit(train$prevalentStroke)) #la moda es 0
train$prevalentStroke<-ifelse(is.na(train$prevalentStroke),0,train$prevalentStroke)

Mode(na.omit(train$prevalentHyp)) #la moda es 0
train$prevalentHyp<-ifelse(is.na(train$prevalentHyp),0,train$prevalentHyp)

Mode(na.omit(train$diabetes)) #la moda es 0
train$diabetes<-ifelse(is.na(train$diabetes),0,train$diabetes)

#TEST (poblacion de prueba)
Mode(na.omit(test$BPMeds)) #la moda es 0
test$BPMeds<-ifelse(is.na(test$BPMeds),0,test$BPMeds)

Mode(na.omit(test$prevalentStroke)) #la moda es 0
test$prevalentStroke<-ifelse(is.na(test$prevalentStroke),0,test$prevalentStroke)

Mode(na.omit(test$prevalentHyp)) #la moda es 0
test$prevalentHyp<-ifelse(is.na(test$prevalentHyp),0,test$prevalentHyp)

Mode(na.omit(test$diabetes)) #la moda es 0
test$diabetes<-ifelse(is.na(test$diabetes),0,test$diabetes)

#Cambiamos NA con la mediana de los datos, para un mejor manejo

#TRAIN (poblacion de referencia)
train<-mutate(train,cigsPerDay=as.numeric(impute(as.data.frame(train$cigsPerDay),what="median")))
train<-mutate(train,totChol=as.numeric(impute(as.data.frame(train$totChol),what="median")))
train<-mutate(train,sysBP=as.numeric(impute(as.data.frame(train$sysBP),what="median")))
train<-mutate(train,diaBP=as.numeric(impute(as.data.frame(train$diaBP),what="median")))
train<-mutate(train,BMI=as.numeric(impute(as.data.frame(train$BMI),what="median")))
train<-mutate(train,heartRate=as.numeric(impute(as.data.frame(train$heartRate),what="median")))
train<-mutate(train,glucose=as.numeric(impute(as.data.frame(train$glucose),what="median")))

#TEST (poblacion de prueba)
test<-mutate(test,cigsPerDay=as.numeric(impute(as.data.frame(test$cigsPerDay),what="median")))
test<-mutate(test,totChol=as.numeric(impute(as.data.frame(test$totChol),what="median")))
test<-mutate(test,sysBP=as.numeric(impute(as.data.frame(test$sysBP),what="median")))
test<-mutate(test,diaBP=as.numeric(impute(as.data.frame(test$diaBP),what="median")))
test<-mutate(test,BMI=as.numeric(impute(as.data.frame(test$BMI),what="median")))
test<-mutate(test,heartRate=as.numeric(impute(as.data.frame(test$heartRate),what="median")))
test<-mutate(test,glucose=as.numeric(impute(as.data.frame(test$glucose),what="median")))


write.csv(test,"test_clean.csv") #Archivo en github
write.csv(train,"train_clean.csv") #Archivo en github

#FILTROS SUBGRUPOS----


#Hicimos subgrupos de las dos bases de datos TRAIN Y TEST para poder hacer comparaciones entre estos 
#y ver si tenían diferencias significativas respecto a las variables independientes (Y)


train1 <- train %>%  #BD con los pacientes de referecia CON EL RIESGO DE ENFERMEDAD
  filter(TenYearCHD==1) 

train2 <- train %>%   #BD con los pacientes de referecia SIN EL RIESGO DE ENFERMEDAD
  filter(TenYearCHD==0) 

trainF <- train1 %>% #BD con las pacientes de referencia MUJERES CON EL RIESGO DE ENFERMEDAD
  filter(sex=="F") 

trainM <- train1 %>%  #BD con los pacientes de referencia HOMBRES CON EL RIESGO DE ENFERMEDAD
  filter(sex=="M") 

testM <- test %>%  #BD de prueba con los pacientes HOMBRES
  filter(sex=="M") 

testF <- test %>% #BD de prueba con las pacientes MUJERES 
  filter(sex=="F") 


#Ahora, para visualizar gráficamente cómo se representa cada variable dentro de la población TRAIN, TRAIN1 y TEST
#realizamos diferentes gráficos para ver %, distribuciones, y ver como cambian las medias.

#Notar que al ser dos poblaciones y múltiples variables fueron muchas las gráficas resultantes, pues es 
#importante para poder plantear la mejor hipótesis.

#GRÁFICOS DE BARRAS TRAIN GENERAL

#Para visualizar qué % de la poblacion de referencia tiene algún factor de riesgo


#¿Qué % de hombres y mujeres conforman la población de refeencia?
ggplot(train,aes(x=factor(sex)))+
  geom_bar(color = "black", fill ="blue",aes(y = (..count..)/sum(..count..)))+
  labs(x="",y="Total",
       title="Población de referencia")+
  scale_x_discrete(labels = c('MUJERES','HOMBRES'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent_format(percent=100,scale = 100))
#Podemos ver que hay más mujeres que hombres en la población de referencia


#Frecuencias de si fuman o no
ggplot(train,aes(x=factor(is_smoking)))+
  geom_bar(color = "black", fill ="red",aes(y = (..count..)/sum(..count..)))+
  labs(x="",y="Total",
       title="Pacientes fumadores y no fumadores")+
  scale_x_discrete(labels = c('No fumadores','Fumadores'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent_format(percent=100,scale = 100))

#Frecuencias de si son hipertensa o no
ggplot(train,aes(x=factor(prevalentStroke)))+
  geom_bar(color = "black", fill ="purple",aes(y = (..count..)/sum(..count..)))+
  labs(x="",y="Total",
       title="Pacientes hipertensos y no hipertensos")+
  scale_x_discrete(labels = c('No hipertensos','Hipertensos'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent_format(percent=100,scale = 100))

#Frecuencias de diabetes
ggplot(train,aes(x=factor(diabetes)))+
  geom_bar(color = "black", fill ="lightblue",aes(y = (..count..)/sum(..count..)))+
  labs(x="",y="Total",
       title="Pacientes diabéticos y no diabéticos")+
  scale_x_discrete(labels = c('Sin diabetes','Con diabetes'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent_format(percent=100,scale = 100))

#Frecuencias de enfermedad crónica
ggplot(train,aes(x=factor(diabetes)))+
  geom_bar(color = "black", fill ="orange",aes(y = (..count..)/sum(..count..)))+
  labs(x="",y="Total",
       title="Pacientes con y sin riesgo de enfermedad")+
  scale_x_discrete(labels = c('Sin riesgo','Con riesgo'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent_format(percent=100,scale = 100))


#De acuerdo a los % vemos que la mayoría de la población DE REFERENCIA no pertenece al grupo con 
#algunos factores de riesgo como el padecer diabetes, hipertensión o ser fumadora. Y como vemos, la mayoría no 
#presenta el riesgo de desarrollar la enfermedad coronaria. 
#Ahora nos preguntamos si habían diferencias de la población total con respecto al sexo, y lo visualizamos 
#con las siguientes gráficas.


#GRAFICOS DE BARRAS BD TRAIN POR SEXO----


#¿Cuantos hombres y cuantas mujeres tienen o no diabetes?
colores<-c("pink","red","aquamarine","blue")
levels(train$sex) <- c("Mujer", "Hombre")
ggplot(train,aes(x=factor(diabetes)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train$sex)+
  labs(x="",y="Total",
       title="Personas con y sin diabetes")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('Sin diabetes','Con diabetes'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuantos hombres y cuantas mujeres toman medicamentos para la presión?
colores<-c("pink","red","aquamarine","blue")
levels(train$sex) <- c("Mujer", "Hombre")
ggplot(train,aes(x=factor(BPMeds)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train$sex)+
  labs(x="",y="Total",
       title="Personas que consumen o no medicamentos")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('No toman Meds','Toman Meds'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuantos hombres y cuantas mujeres son o no hipertensas?
colores<-c("pink","red","aquamarine","blue")
levels(train$sex) <- c("Mujer", "Hombre")
ggplot(train,aes(x=factor(prevalentHyp)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train$sex)+
  labs(x="",y="Total",
       title="Personas con o sin hipertensión")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('No Hiper','Hipertensas'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuantos hombres y cuantas mujeres ha sufrido infartos?
colores<-c("pink","red","aquamarine","blue")
levels(train$sex) <- c("Mujer", "Hombre")
ggplot(train,aes(x=factor(prevalentStroke)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train$sex)+
  labs(x="",y="Total",
       title="Personas que han sufrido o no infartos")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('NO','SI'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuantos hombres y cuantas mujeres fuman?
colores<-c("pink","red","aquamarine","blue")
levels(train$sex) <- c("Mujer", "Hombre")
ggplot(train,aes(x=factor(is_smoking)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train$sex)+
  labs(x="",y="Total",
       title="Personas fumadoras y no fumadoras")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('NO','SI'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuántos hombres y cuantas mujeres tienen riesgo de sufrir la enfermedad?
colores<-c("pink","red","aquamarine","blue")
levels(train$sex) <- c("Mujer", "Hombre")
ggplot(train,aes(x=factor(TenYearCHD)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train$sex)+
  labs(x="",y="Total",
       title="Personas con y sin riesgo de enfermedad en 10 años")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('Sin riesgo','Con riesgo'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Hasta ahora podemos ver que la mayoría de la población es femenina, y que además las 
#proporciones de personas con los factores de riesgo se ven muy distintos. Algo que también sucede con
#las personas que presentan o no el riesgo de enfermedad. 

#Con relación a la última gráfica, optamos pot tomar el subgrupo de personas que SÍ tienen el riesgo
#de desarrollar la enfermedad coronaria (TRAIN1), y visualizar cómo se distrbuye la población con respecto al 
#sexo y por cada variable independiente.


#GRAFICOS BD TRAIN1 (PACIENTES CON RIESGO)----

#¿Cuántos hombres y cuántas mujeres sufren el riesgo de enfermedad?
ggplot(train1,aes(x=factor(sex)))+
  geom_bar(color = "black", fill ="blue",aes(y = (..count..)/sum(..count..)))+
  labs(x="",y="Total",
       title="Pacientes con riesgo de enfermedad")+
  scale_x_discrete(labels = c('MUJERES','HOMBRES'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent_format(percent=100,scale = 100))
#A pesar de que en la población total de referencia la mayoria son mujeres, aquí podemos ver 
#que de las personas que tienen el riesgo de desarrollar la enfermedad coronaria, la mayoría son hombres.
#Por lo que desde aquí podríamos hipotetizar que el sexo, es un factor involucrado en el riesgo de desarrollar
#la enfermedad coronaria crónica.


#¿Cuantos hombres y cuantas mujeres tienen o no diabetes?
colores<-c("pink","purple","aquamarine","blue")
levels(train1$sex) <- c("Mujer", "Hombre")
ggplot(train1,aes(x=factor(diabetes)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train1$sex)+
  labs(x="",y="Total",
       title="Pacientes con y sin diabetes")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('Sin diabetes','Con diabetes'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuantos hombres y cuantas mujeres toman medicamentos para la presión?
colores<-c("pink","purple","aquamarine","blue")
levels(train1$sex) <- c("Mujer", "Hombre")
ggplot(train1,aes(x=factor(BPMeds)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train1$sex)+
  labs(x="",y="Total",
       title="Pacientes que consumen o no medicamentos")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('No toman Meds','Toman Meds'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuantos hombres y cuantas mujeres son o no hipertensas?
colores<-c("pink","purple","aquamarine","blue")
levels(train1$sex) <- c("Mujer", "Hombre")
ggplot(train1,aes(x=factor(prevalentHyp)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train1$sex)+
  labs(x="",y="Total",
       title="Pacientes con o sin hipertensión")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('No Hiper','Hipertensas'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#¿Cuantos hombres y cuantas mujeres han sufrido infartos?
colores<-c("pink","purple","aquamarine","blue")
levels(train1$sex) <- c("Mujer", "Hombre")
ggplot(train1,aes(x=factor(prevalentStroke)))+
  geom_bar(color = "black", fill =colores)+
  facet_wrap(train1$sex)+
  labs(x="",y="Total",
       title="Pacientes que han sufrido o no infartos")+
  facet_grid(.~sex)+
  scale_x_discrete(labels = c('NO','SI'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#De éstas gráficas vemos que a pesar de que hay más hombres con riesgo de desarrollar la enfermedad,
#hay más mujeres que sufren de hipertensión, que se medican, y que han sufrido ataques cardiacos previos en
#comparación con los hombres. 

#Por lo que otra hipótesis que surge es que, además de que el factor sexo está involucrado en el riesgo de 
#enfermedad coronaria, también lo está el tener hipertensión y haber sufrido ataques cardiaco previos.


#Se realizaron los histogramas de frecuencias de cada una de las variables cuantitativas dentro de cada población:
#Train (Poblacion de referencia), Train1 (Población con riersgo de enfermedad) y Test (población de prueba).
#Esto para tener idea de cómo están distribuidos los datos y si siguen algun tipo de distribución normal o t.

#HISTOGRAMAS TRAIN----

train %>%
  ggplot() + 
  aes(age) +
  geom_histogram(binwidth = 2, col="black", fill = "brown") + 
  geom_vline(xintercept = mean(train$age), linetype="dashed", color = "black") + 
  ggtitle("Edades en pacientes de referencia") +
  ylab("Frecuencia") +
  xlab("EDAD") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13))

train %>%
  ggplot() + 
  aes(BMI) +
  geom_histogram(binwidth =2, col="black", fill = "blue") + 
  geom_vline(xintercept = mean(train$BMI), linetype="dashed", color = "black") + 
  ggtitle("IMC en pacientes de referencia") +
  ylab("Frecuencia") +
  xlab("IMC") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))  

train %>%
  ggplot() + 
  aes(totChol) +
  geom_histogram(binwidth = 15, col="black", fill = "yellow") + 
  geom_vline(xintercept = mean(train$totChol), linetype="dashed", color = "black") + 
  ggtitle("Colesterol total en pacientes de referencia") +
  ylab("Frecuencia") +
  xlab("Niveles de colesterol") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) 

train %>%
  ggplot() + 
  aes(heartRate) +
  geom_histogram(binwidth = 5, col="black", fill = "red") + 
  geom_vline(xintercept = mean(train$heartRate), linetype="dashed", color = "black") + 
  ggtitle("Frecuencia cardiaca en pacientes de referencia") +
  ylab("Frecuencia") +
  xlab("Frecuencia cardiaca") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))  

train %>%
  ggplot() + 
  aes(glucose) +
  geom_histogram(binwidth = 10, col="black", fill = "orange") + 
  geom_vline(xintercept = mean(train$glucose), linetype="dashed", color = "black") + 
  ggtitle("Glucosa en pacientes de referencia") +
  ylab("Frecuencia") +
  xlab("Glucosa") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13))  

train %>%
  ggplot() + 
  aes(diaBP) +
  geom_histogram(binwidth =5, col="black", fill = "pink") + 
  geom_vline(xintercept = mean(train$diaBP), linetype="dashed", color = "black") + 
  ggtitle("Presión diastólica en pacientes de referencia") +
  ylab("Frecuencia") +
  xlab("presión diastólica") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) 

train %>%
  ggplot() + 
  aes(sysBP) +
  geom_histogram(binwidth = 10, col="black", fill = "purple") + 
  geom_vline(xintercept = mean(train$sysBP), linetype="dashed", color = "black") + 
  ggtitle("Presión sistólica en pacientes de referencia") +
  ylab("Frecuencia") +
  xlab("presión sistólica") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))  

#HISTOGRAMAS TRAIN1----

train1 %>%
  ggplot() + 
  aes(age) +
  geom_histogram(binwidth = 3, col="black", fill = "brown") + 
  geom_vline(xintercept = mean(train1$age), linetype="dashed", color = "black") + 
  ggtitle("Edades en pacientes con riesgo de enfermedad") +
  ylab("Frecuencia") +
  xlab("EDAD") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13))  

train1 %>%
  ggplot() + 
  aes(BMI) +
  geom_histogram(binwidth = 2, col="black", fill = "blue") + 
  geom_vline(xintercept = mean(train1$BMI), linetype="dashed", color = "black") + 
  ggtitle("IMC en pacientes con riesgo de enfermedad") +
  ylab("Frecuencia") +
  xlab("IMC") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13))  

train1 %>%
  ggplot() + 
  aes(totChol) +
  geom_histogram(binwidth = 15, col="black", fill = "yellow") + 
  geom_vline(xintercept = mean(train1$totChol), linetype="dashed", color = "black") + 
  ggtitle("Colesterol total en pacientes con riesgo de enfermedad") +
  ylab("Frecuencia") +
  xlab("Niveles de colesterol") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

train1 %>%
  ggplot() + 
  aes(heartRate) +
  geom_histogram(binwidth = 5, col="black", fill = "red") + 
  geom_vline(xintercept = mean(train1$heartRate), linetype="dashed", color = "black") + 
  ggtitle("Frecuencia cardiaca en pacientes con riesgo de enfermedad") +
  ylab("Frecuencia") +
  xlab("Frecuencia cardiaca") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 10))  

train1 %>%
  ggplot() + 
  aes(glucose) +
  geom_histogram(binwidth = 10, col="black", fill = "orange") + 
  geom_vline(xintercept = mean(train1$glucose), linetype="dashed", color = "black") + 
  ggtitle("Glucosa en pacientes con riesgo de enfermedad") +
  ylab("Frecuencia") +
  xlab("Glucosa") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) 

train1 %>%
  ggplot() + 
  aes(diaBP) +
  geom_histogram(binwidth = 5, col="black", fill = "pink") + 
  geom_vline(xintercept = mean(train1$diaBP), linetype="dashed", color = "black") + 
  ggtitle("Presión diastólica en pacientes con riesgo de enfermedad") +
  ylab("Frecuencia") +
  xlab("presión diastólica") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

train1 %>%
  ggplot() + 
  aes(sysBP) +
  geom_histogram(binwidth = 10, col="black", fill = "purple") + 
  geom_vline(xintercept = mean(train1$sysBP), linetype="dashed", color = "black") + 
  ggtitle("Presión sistólica en pacientes de referencia con DC") +
  ylab("Frecuencia") +
  xlab("presión sistólica") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))  

#HISTOGRAMAS TEST----

test %>%
  ggplot() + 
  aes(age) +
  geom_histogram(binwidth = 2, col="black", fill = "brown") + 
  geom_vline(xintercept = mean(test$age), linetype="dashed", color = "black") + 
  ggtitle("Edades en pacientes de prueba") +
  ylab("Frecuencia") +
  xlab("EDAD") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13))

test %>%
  ggplot() + 
  aes(BMI) +
  geom_histogram(binwidth = 2, col="black", fill = "orange") + 
  geom_vline(xintercept = mean(test$BMI), linetype="dashed", color = "black") + 
  ggtitle("IMC de pacientes de prueba") +
  ylab("Frecuencia") +
  xlab("IMC") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))  

test %>%
  ggplot() + 
  aes(totChol) +
  geom_histogram(binwidth = 15, col="black", fill = "yellow") + 
  geom_vline(xintercept = mean(test$totChol), linetype="dashed", color = "black") + 
  ggtitle("Colesterol total en pacientes de prueba") +
  ylab("Frecuencia") +
  xlab("Niveles de colesterol") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

test %>%
  ggplot() + 
  aes(heartRate) +
  geom_histogram(binwidth = 5, col="black", fill = "red") + 
  geom_vline(xintercept = mean(test$heartRate), linetype="dashed", color = "black") + 
  ggtitle("Frecuencia cardiaca en pacientes de prueba") +
  ylab("Frecuencia") +
  xlab("Frecuencia cardiaca") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) 

test %>%
  ggplot() + 
  aes(glucose) +
  geom_histogram(binwidth = 10, col="black", fill = "orange") + 
  geom_vline(xintercept = mean(test$glucose), linetype="dashed", color = "black") + 
  ggtitle("Glucosa en pacientes de prueba") +
  ylab("Frecuencia") +
  xlab("Glucosa") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

test %>%
  ggplot() + 
  aes(diaBP) +
  geom_histogram(binwidth = 5, col="black", fill = "pink") + 
  geom_vline(xintercept = mean(test$diaBP), linetype="dashed", color = "black") + 
  ggtitle("Presión diastólica en pacientes de prueba") +
  ylab("Frecuencia") +
  xlab("presión diastólica") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

test %>%
  ggplot() + 
  aes(sysBP) +
  geom_histogram(binwidth = 10, col="black", fill = "purple") + 
  geom_vline(xintercept = mean(test$sysBP), linetype="dashed", color = "black") + 
  ggtitle("Presión sistólica en pacientes de prueba") +
  ylab("Frecuencia") +
  xlab("presión sistólica") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))


#Vemos que la mayoría de las variables siguen una distribución muy parecida entre cada población.
#Todas las variables exepto la de GLUCOSA, parecen seguir una distribución tipo t de student. Con lo 
#que sabríamos qué tipo de análisis aplicar al querer comparar las diferencias entre las medias por ejemplo.

#Mediante la visualización de las medias con gráficos de cajas, podemos ver si hay alguna diferencia entre 
#las poblaciones con respecto al género, que como hemos visto se distribuyen de manera diferente los datos 
#en relación al sexo, y entre las diferentes variables independientes.

#A continuación solo graficamos las variables de la población TRAIN1 (población con riesgo de enfermedad) y
#la población TEST (de prueba), para visualizar si se diferencían de alguna forma las medias entre cada población
#y obtener alguna hipótesis de qué variables podrían estar relacionadas al desarrollo de la enfermedad.


#BOXPLOTS TRAIN1----

#boxplot de FREC. CARDIACA por sexo 
colores.box<-c("pink","lightblue")
ggplot(train1,aes(sex,heartRate))+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Frecuencia cardiaca")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para IMC por sexo
ggplot(train1,aes(sex,BMI),y=BMI)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="IMC")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para colesterol por sexo
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

#Boxplot para PRESIÓN DIASTÓLICA por sexo
ggplot(train1,aes(sex,diaBP),y=diaBP)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Presion diastolica")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para num de cigarros al día por sexo
ggplot(train1,aes(sex,cigsPerDay),y=cigsPerDay)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Cigarros al día")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#BOXPLOTS TEST----

#boxplot de FREC. CARDIACA por sexo 
colores.box<-c("pink","lightblue")
ggplot(test,aes(sex,heartRate),y=heartRate)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Frecuencia cardiaca")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#Boxplot para IMC por sexo
ggplot(test,aes(sex,BMI),y=BMI)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="IMC")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para colesterol por sexo
ggplot(test,aes(sex,totChol),y=totChol)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Nivel de colesterol")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para glucosa por sexo
ggplot(test,aes(sex,glucose),y=glucose)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Nivel de glucosa")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para presión sistolica por sexo
ggplot(test,aes(sex,sysBP),y=sysBP)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Presión sistolica")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para PRESIÓN DIASTÓLICA por sexo
ggplot(test,aes(sex,diaBP),y=diaBP)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Presion diastolica")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot para num de cigarros al día por sexo
ggplot(test,aes(sex,cigsPerDay),y=cigsPerDay)+
  geom_boxplot(color = "black", fill =colores.box)+
  labs(x="Sexo",y="Cigarros al día")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#A simple vista podríamos decir que no se observan marcadas diferencias entre las medias de cada factor de riesgo
#por cada población. Sin embargo hicimos uso de otro tipo de gráficos para poder identificar qué variables
#podrían estar más asociadas al riesgo de desarrollo de la enfermedad. Se muestran a continuación.


#GRAFICOS DE INTERACCIÓN TRAIN, TRAIN1 & TEST----

#CONVERTIR VARIABLE CATEGÓRICA EN NOMINAL PARA PODER GRAFICAR.
train<-mutate(train,is_smoking=as.character(train$is_smoking))
for (i  in 1:length(train$is_smoking)) {
  if (train$is_smoking[i]=="YES") {train$is_smoking[i]=1}
  else {train$is_smoking[i]=0}
}

test<-mutate(test,is_smoking=as.character(test$is_smoking))
for (i  in 1:length(test$is_smoking)) {
  if (test$is_smoking[i]=="YES") {test$is_smoking[i]=1}
  else {test$is_smoking[i]=0}
}


#COMPORTAMIENTO FUMADORES Y NO FUM CON FRECUENCIA CARDIACA
ggplot(train1, aes(x = is_smoking, y = heartRate, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(train2, aes(x = is_smoking, y = heartRate, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(test, aes(x = is_smoking, y = heartRate, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMIENTO DIABETICOS Y NO DIABETICOS VS COLESTEROL TOTAL
ggplot(train1, aes(x = diabetes, y = totChol, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(train2, aes(x = diabetes, y = totChol, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(test, aes(x = diabetes, y = totChol, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMIENTO PERSONAS SIN Y CON ATAQUES VS EDAD
ggplot(train1, aes(x = prevalentStroke, y = age, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(train2, aes(x = prevalentStroke, y = age, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(test, aes(x = prevalentStroke, y = age, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMIENTO HIPERTENSAS Y NO HIPERTENSAS VS COLESTEROL TOTAL
ggplot(train1, aes(x = prevalentHyp, y = totChol, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(train2, aes(x = prevalentHyp, y = totChol, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

ggplot(test, aes(x = prevalentHyp, y = totChol, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()


#COMPORTAMINETO PACIENTES CON Y SIN RIESGO VS FRECUENCIA CARDIACA
ggplot(train, aes(x = TenYearCHD, y = heartRate, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMINETO PACIENTES CON Y SIN RIESGO CON COLESTEROL TOTAL
ggplot(train, aes(x = TenYearCHD, y = totChol, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMINETO PACIENTES CON Y SIN RIESGO CON GLUCOSA
ggplot(train, aes(x = TenYearCHD, y = glucose, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMINETO PACIENTES CON Y SIN RIESGO CON CIGARROS AL DIA
ggplot(train, aes(x = TenYearCHD, y = cigsPerDay, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMINETO PACIENTES CON Y SIN RIESGO CON INDICE DE MASA CORPORAL
ggplot(train, aes(x = TenYearCHD, y = BMI, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMINETO PACIENTES CON Y SIN RIESGO CON PRESION SISTOLOCA
ggplot(train, aes(x = TenYearCHD, y = sysBP, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()

#COMPORTAMINETO PACIENTES CON Y SIN RIESGO CON PRESION DIASTOLICA
ggplot(train, aes(x = TenYearCHD, y = diaBP, group = sex, color = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  theme_bw()


#En estos gráficos podemos corroborar que cuando el paciente tiene riesgo de desarrollar la enfermedad 
#cardiaca, los valores en los niveles de colesterol, el IMC, la presión sanguínea, la glucosa y la frecuencia 
#cardiaca, se ven aumentados.
#Pero además, vemos que las mujeres alcanzan niveles más altos de algunas variables a diferencia de los hombres.
#Con lo que analizamos las diferencias entre las medias de las variables dentro de la población de referencia.


#DOCIMACIA DE HIPÓTESIS----


#Diferencias entre poblaciones respecto a niveles de BMI

t.test(train$BMI~train$sex)#sig < PACIENTES DE REF, DIF EN Índice de masa corporal RESPECTO AL SEXO
t.test(train1$BMI~train1$sex)# PACIENTES DE REF CON DC, DIF EN Índice de masa corporal RESPECTO AL SEXO
t.test(train2$BMI~train2$sex)#sig < PACIENTES DE REF SIN DC, DIF EN Índice de masa corporal RESPECTO AL SEXO
t.test(test$BMI~test$sex)# PACIENTES DE PRUEBA, DIF EN Índice de masa corporal RESPECTO AL SEXO

t.test(train$BMI,test$BMI) # PACIENTES DE REF vs PACIENTES PRUEBA
t.test(train1$BMI,test$BMI) #sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
t.test(train1$BMI,train2$BMI) #sig < PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
t.test(train2$BMI,test$BMI) #no sig < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA


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


#Diferencias entre poblaciones respecto a niveles de glucose


wilcox.test(train$glucose~train$sex, exact=F, correct=F)# PACIENTES DE REF, DIF EN Glucosa RESPECTO AL SEXO
wilcox.test(train1$glucose~train1$sex, exact=F, correct=F)# PACIENTES DE REF CON DC, DIF EN Glucosa RESPECTO AL SEXO 
wilcox.test(train2$glucose~train2$sex, exact=F, correct=F)# PACIENTES DE REF SIN DC, DIF EN Glucosa RESPECTO AL SEXO
wilcox.test(test$glucose~test$sex, exact=F, correct=F)#sig < PACIENTES DE PRUEBA, DIF EN Glucosa RESPECTO AL SEXO 

wilcox.test(trainF$glucose,testF$glucose, exact=F, correct=F)#sig > PACIENTES DE REF CON DC MU vs PACIENTES DE PRUEBA MUJ
wilcox.test(trainM$glucose,testM$glucose, exact=F, correct=F)# PACIENTES DE REF CON DC H vs PACIENTES DE PRUEBA H

wilcox.test(train$glucose,test$glucose, exact=F, correct=F)# PACIENTES DE REF vs PACIENTES PRUEBA
wilcox.test(train1$glucose,test$glucose, exact=F, correct=F)#sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
wilcox.test(train1$glucose,train2$glucose, exact=F, correct=F) #sig < PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
wilcox.test(train2$glucose,test$glucose, exact=F, correct=F) #no < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA


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


#Diferencias entre poblaciones respecto a valores de SysBP

t.test(train$sysBP~train$sex)#sig < PACIENTES DE REF, DIF EN Presión sistólica RESPECTO AL SEXO
t.test(train1$sysBP~train1$sex)#sig < PACIENTES DE REF CON DC, DIF EN Presión sistólica RESPECTO AL SEXO  
t.test(train2$sysBP~train2$sex)#sig < PACIENTES DE REF SIN DC, DIF EN Presión sistólica RESPECTO AL SEXO 
t.test(test$sysBP~test$sex)# PACIENTES DE PRUEBA, DIF EN Presión sistólica RESPECTO AL SEXO  

t.test(trainF$sysBP,testF$sysBP)#sig < PACIENTES DE REF CON DC MU vs PACIENTES DE PRUEBA MUJ
t.test(trainM$sysBP,testM$sysBP)#sig < PACIENTES DE REF CON DC H vs PACIENTES DE PRUEBA H

t.test(train$sysBP,test$sysBP)# PACIENTES DE REF vs PACIENTES PRUEBA 
t.test(train1$sysBP,test$sysBP)#sig < PACIENTES DE REF CON DC vs PACIENTES PRUEBA
t.test(train1$sysBP,train2$sysBP)#sig <  PACIENTES DE REF CON DC vs PACIENTES DE REF SIN DC
t.test(train2$sysBP,test$sysBP) #no < PACIENTES DE REF SIN DC vs PACIENTES PRUEBA


#Encontramos que existen diferencias significativas con respecto al sexo, y entre la población 
#con y sin riesgo de desarrollar la enfermedad.

#Con esto y todo lo anterior, llegamos a la hipótesis de que dependiendo el sexo, haber sufrido 
#ataques cardiacos y tener niveles altos de colesterol, glucosa y presión sanguínea, son los principales 
#factores que llevan al desarrollo de la enfermedad coronaria crónica.
