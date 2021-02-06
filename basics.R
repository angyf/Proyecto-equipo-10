install.packages("ggplot2")

# -----------------------------------------------------------------------------------------


suppressMessages(suppressWarnings(library(dplyr)))


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

# Fumadores con diabetes, hipertensos, tomando medicamento para presion sanguinea alta y alto colasterol
personas_mala_condicion <- filter( cardioData, is_smoking == 'YES' & diabetes == 1 & BPMeds == 1 & totChol >= 130 & prevalentHyp == 1) 


# Nivel de colesterol total	Categoría
# Entre 100 y 129 mg/dL	Casi óptimo o por encima del valor óptimo
# Entre 130 y 159 mg/dL	Límite superior del rango normal
# Entre 160 y 189 mg/dL	Alto
# 190 mg/dL o más	Muy alto

personas_con_colasterol_aceptable <- filter( cardioData, totChol >= 100 & totChol <= 129)    # 6 Observaciones
personas_con_colasterol_limite_superior <- filter( cardioData, totChol >= 130 & totChol <= 159)    # 67
personas_con_colasterol_alto <- filter( cardioData, totChol >= 160 & totChol <= 189)    # 369 Observaciones
personas_con_colasterol_muy_alto <- filter( cardioData, totChol >= 190 )    # 2910


m_personas_con_colasterol_aceptable <- filter( cardioData, totChol >= 100 & totChol <= 129 & sex == 'M' )    # 6 Observaciones
m_personas_con_colasterol_limite_superior <- filter( cardioData, totChol >= 130 & totChol <= 159 & sex == 'M')    # 67
m_personas_con_colasterol_alto <- filter( cardioData, totChol >= 160 & totChol <= 189 & sex == 'M')    # 369 Observaciones
m_personas_con_colasterol_muy_alto <- filter( cardioData, totChol >= 190 & sex == 'M')    # 2910


f_personas_con_colasterol_aceptable <- filter( cardioData, totChol >= 100 & totChol <= 129 & sex == 'F' )    # 6 Observaciones
f_personas_con_colasterol_limite_superior <- filter( cardioData, totChol >= 130 & totChol <= 159 & sex == 'F')    # 67
f_personas_con_colasterol_alto <- filter( cardioData, totChol >= 160 & totChol <= 189 & sex == 'F')    # 369 Observaciones
f_personas_con_colasterol_muy_alto <- filter( cardioData, totChol >= 190 & sex == 'F')    # 2910

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


f_smoking_age_30_40  <- filter( age_30_40, is_smoking == 'YES' & sex == 'F' ) # 369 Observaciones
f_smoking_age_40_50  <- filter( age_40_50, is_smoking == 'YES' & sex == 'F') # 834 Observaciones
f_smoking_age_50_60  <- filter( age_50_60, is_smoking == 'YES' & sex == 'F') # 497 Observaciones *
f_smoking_age_60_70  <- filter( age_60_70, is_smoking == 'YES' & sex == 'F') # 175 Observaciones *
f_smoking_age_70_80  <- filter( age_70_80, is_smoking == 'YES' & sex == 'F' ) # 0 Observaciones   *


h_smoking_age_30_40  <- filter( age_30_40, is_smoking == 'YES' & sex == 'M' ) # 369 Observaciones
h_smoking_age_40_50  <- filter( age_40_50, is_smoking == 'YES' & sex == 'M') # 834 Observaciones
h_smoking_age_50_60  <- filter( age_50_60, is_smoking == 'YES' & sex == 'M') # 497 Observaciones *
h_smoking_age_60_70  <- filter( age_60_70, is_smoking == 'YES' & sex == 'M') # 175 Observaciones *
H_smoking_age_70_80  <- filter( age_70_80, is_smoking == 'YES' & sex == 'M' ) # 0 Observaciones   *


# Por rangos de edades en 10 y no fumadores
no_smoking_age_30_40  <- filter( age_30_40, is_smoking == 'NO' ) # 235 Observaciones
no_smoking_age_40_50  <- filter( age_40_50, is_smoking == 'NO' ) # 597 Observaciones
no_smoking_age_50_60  <- filter( age_50_60, is_smoking == 'NO' ) # 659 Observaciones *
no_smoking_age_60_70  <- filter( age_60_70, is_smoking == 'NO' ) # 382 Observaciones *
no_smoking_age_70_80  <- filter( age_70_80, is_smoking == 'NO' ) # 2 Observaciones   *


# Por rangos de edades en 10 y no fumadores
h_no_smoking_age_30_40  <- filter( age_30_40, is_smoking == 'NO' & sex == 'M') # 235 Observaciones
h_no_smoking_age_40_50  <- filter( age_40_50, is_smoking == 'NO' & sex == 'M' ) # 597 Observaciones
h_no_smoking_age_50_60  <- filter( age_50_60, is_smoking == 'NO' & sex == 'M') # 659 Observaciones *
h_no_smoking_age_60_70  <- filter( age_60_70, is_smoking == 'NO' & sex == 'M') # 382 Observaciones *
h_no_smoking_age_70_80  <- filter( age_70_80, is_smoking == 'NO' & sex == 'M') # 2 Observaciones   *

# Por rangos de edades en 10 y no fumadores
f_no_smoking_age_30_40  <- filter( age_30_40, is_smoking == 'NO' & sex == 'F') # 235 Observaciones
f_no_smoking_age_40_50  <- filter( age_40_50, is_smoking == 'NO' & sex == 'F') # 597 Observaciones
f_no_smoking_age_50_60  <- filter( age_50_60, is_smoking == 'NO' & sex == 'F') # 659 Observaciones *
f_no_smoking_age_60_70  <- filter( age_60_70, is_smoking == 'NO' & sex == 'F') # 382 Observaciones *
f_no_smoking_age_70_80  <- filter( age_70_80, is_smoking == 'NO' & sex == 'F') # 2 Observaciones   *



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
                 BPMeds = BPMeds,# Blood Pressure Meds
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
