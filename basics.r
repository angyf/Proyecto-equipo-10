suppressMessages(suppressWarnings(library(dplyr)))

install.packages("ggplot2")

library(ggplot2)


# -----------------------------------------------------------------------------------------

setwd("/Users/ninobomba/Downloads/CardioVascularDataset")

cardioData <- read.csv("train.csv")

# -----------------------------------------------------------------------------------------

head( cardioData )

str( cardioData )

dim( cardioData )

View( cardioData )

# -----------------------------------------------------------------------------------------

# Verificar el numero de datos marcados como NA - 

na_cases <- complete.cases( cardioData )
sum( na_cases ) 
cardioData <- na.omit( cardioData ) # clean up data = 2927, cardio Data 3390

# -----------------------------------------------------------------------------------------

fumadores_diabeticos <- filter( cardioData, is_smoking == 'YES' & diabetes == 1  ) # 29 Observaciones
women_fumadores_diabeticos <- filter( cardioData, is_smoking == 'YES' & sex == 'F' ) # 16 Observaciones
men_fumadores_diabeticos <- filter( cardioData, is_smoking == 'YES' & sex == 'M' ) # 13 Observaciones

fumadores_no_diabeticos <- filter( cardioData, is_smoking == 'YES' & diabetes == 0 ) # 29 Observaciones
women_fumadores_no_diabeticos <- filter( cardioData, is_smoking == 'YES' & sex == 'F' ) # 16 Observaciones
men_fumadores_no_diabeticos <- filter( cardioData, is_smoking == 'YES' & sex == 'M' ) # 13 Observaciones


# Por rangos de edades en 10
age_30_40  <- filter( cardioData, age > 29 & age < 41 ) # 604 Observaciones
age_40_50  <- filter( cardioData, age > 39 & age < 51 ) # 1431 Observaciones
age_50_60  <- filter( cardioData, age > 49 & age < 61 ) # 1156 Observaciones
age_60_70  <- filter( cardioData, age > 59 & age < 71 ) # 557 Obervaciones
age_70_80  <- filter( cardioData, age > 69 & age < 81 ) # 2 Obervaciones


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
ggplot( cardioData, aes( x = age, y = BMI )) + 
  geom_point() +  
  xlab('Edad') +  
  ylab('Indice de masa corporal')

# Fumadores diabeticos, edad y cigarrillos al dia
ggplot( fumadores_diabeticos, aes( x = age, y = cigsPerDay ) ) + 
  geom_point() +  
  xlab('Edad') +  
  ylab('Cigarros al dia')

# Fumadores diabeticos, edad y cigarrillos al dia
ggplot( fumadores_no_diabeticos, aes( x = age, y = cigsPerDay )) + 
  geom_point() +
  xlab('Edad') +  
  ylab('Cigarros al dia')

names(fumadores_no_diabeticos)

summary( cardioData )

hist(cardioData$age, 
     breaks = seq(30, 70), 
     main = "Histograma Edades",
     xlab = "Rango Edades",
     ylab = "Frecuencia")

cardioData %>%
  ggplot() + 
  aes(cardioData$age) +
  geom_histogram(binwidth = 10, col="black", fill = "blue") + 
  ggtitle("Histograma de Edades") +
  ylab("Frecuencia") +
  xlab("Edades") + 
  theme_light()


ggplot(cardioData, aes(x = age, y = cigsPerDay, fill = diabetes)) +
  geom_boxplot() +
  ggtitle("Boxplots") +
  xlab("Edad") +
  ylab("Cigarros al dia")




# -----------------------------------------------------------------------------------------

cardio <- rename(cardioData, 
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
