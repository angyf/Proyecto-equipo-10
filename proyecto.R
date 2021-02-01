library(dplyr)
library(effects)

test<-read.csv("proyecto/test.csv")
train<-read.csv("proyecto/train.csv")

train<-mutate(train,sex=as.character(sex))

#datos<-train%>% filter(TenYearCHD==1)

#convertimos la variable categorica a numerica
for (i  in 1:length(train$sex)) {
        if ((train$sex[i])=="M") {train$sex[i]=0}
        else {train$sex[i]=1}
        }

attach(train)
#Modelo 1 
mol1<-glm(TenYearCHD~age+sex+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+
            totChol+sysBP+diaBP+BMI+heartRate+glucose, family = binomial())
summary(mol1)
#Nos daba un mejor modelo considerando un AIC menor (Criterio de información de Akaike)
step(mol1, test="LRT")

#modelo 2 quitando la variable diabetes por ya tener el nivel de glucosa
plot(train$diabetes,train$glucose)
mol2<-update(mol1, ~.-diabetes)
step(mol2, test="LRT")
summary(mol2)

#modelo 3 quitando la variable presión diaestolica  
mol3<-update(mol2, ~.-diaBP)
summary(mol3)

#plot(mol2$fitted.values, TenYearCHD)
#Menor AIC ->mejor modelo

#coeficientes obtenidos para el modelo
mol3$coefficients

#plot(allEffects(mol3))

#datos para hacer pruebas
prueba<-select(train,age,sex,cigsPerDay, BPMeds,prevalentStroke,prevalentHyp,totChol,sysBP,BMI,heartRate,glucose,TenYearCHD)
prueba<-mutate(prueba,sex=as.integer(sex))

 #pruebas

#(Intercept)             age            sex1      cigsPerDay          BPMeds prevalentStroke 
prueba$probabilidad<-exp(-8.343765370+0.065758138 *prueba$age   -0.496583121*prueba$sex+prueba$cigsPerDay*0.024738249+0.108150318 *prueba$BPMeds+prueba$prevalentStroke*0.901896518+ 
#prevalentHyp         totChol           sysBP             BMI       heartRate         glucose 
0.146124270 *prueba$prevalentHyp+    0.003102936 *prueba$totChol+    0.013588916 * prueba$sysBP+  0.002948087 *prueba$BMI  -0.003585844*prueba$heartRate+     0.009044889*prueba$glucose )

#pruebas sin sentido
dim(prueba%>%filter(probabilidad>1))
  