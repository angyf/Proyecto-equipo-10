
library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)

#Esta parte es el análogo al ui.R
ui <- 
    
    fluidPage(
        dashboardPage( skin = "blue",
                       
                       dashboardHeader(title = "Proyecto"),
                       
                       dashboardSidebar(
                           #pestanas
                           sidebarMenu(
                               
                               menuItem("Datos de entrenamiento", tabName = "data_table", icon = icon("table")),
                               menuItem("Boxplot", tabName = "boxp", icon = icon("bar-chart")),
                               menuItem("Descripción de población", tabName = "im1", icon = icon("bar-chart")),
                               menuItem("Descripción población por sexo", tabName = "im2", icon = icon("bar-chart")),
                               menuItem("Descripción población con riesgo", tabName = "im3", icon = icon("bar-chart")),
                               menuItem("Análisis exploratorio", tabName = "im4", icon = icon("bar-chart")),
                               menuItem("Mujeres con predicción de riesgo",tabName="datam",icon=icon("table")),
                               menuItem("Hombres con predicción de riesgo",tabName="datah",icon=icon("table"))
                           )
                           
                       ),
                       
                       dashboardBody(
                           
                           tabItems(
                               
                               #mostramos el dataframe de entrenamiento de los modelos
                               tabItem(tabName = "data_table",
                                       fluidRow(        
                                           titlePanel(HTML('<h2><center>Base de datos</h2>')),
                                           dataTableOutput ("data_table")
                                       )
                               ),
                               # Boxplots para análisis
                               #el ususario puede elegir la gráfica a mostrar
                               tabItem(tabName = "boxp",
                                       fluidRow( 
                                           titlePanel(HTML('<h2><center>Boxplot</h2>')), 
                                           selectInput("x", "Seleccione ",
                                                       choices = c("Frecuencia_cardiaca","Nivel_de_colesterol", "Nivel_de_glucosa","Presion_sistolica") ),
                                           div(plotOutput("plot", height = 450, width = 600),align="center")
                                       )
                                       
                                   #Graficas del nálisis descriptivo de la población  general   
                                   #el ususario puede elegir la gráfica a mostrar
                               ),
                              tabItem(tabName = "im1",
                                       fluidRow( 
                                           titlePanel(HTML('<h2><center>Población general</h2>')), 
                                           selectInput("ima1", "Seleccione ",
                                                       choices = c("Población de referencia","Fumadores y no fumadores",
                                                                   "Hipertensos y no hipertensos","Diabeticos y no diabeticos",
                                                                   "Pacientes con y sin riesgo")) ,
                                           div( uiOutput("imag1"),align="center")
                                           
                                           
                                           
                                       )
                               ),
                              #Graficas del análisis descriptivo de la población general, por sexo
                              #el ususario puede elegir la gráfica a mostrar
                               tabItem(tabName = "im2",
                                       fluidRow( 
                                           titlePanel(HTML('<h2><center>Población general</h2>')), 
                                           selectInput("ima2", "Seleccione ",
                                                       choices = c("Con y sin hipertension","Con y sin diabetes",
                                                                   "Con y sin riesgo")) ,
                                           div( uiOutput("imag2"),align="center")
                                         
                                           
                                           
                               )
                       ),
                      #Gráficas del analisis de la población con riesgo
                      #el ususario puede elegir la gráfica a mostrar 
                       tabItem(tabName = "im3",
                               fluidRow( 
                                   titlePanel(HTML('<h2><center>Población con riesgo</h2>')), 
                                   selectInput("ima3", "Seleccione ",
                                               choices = c("Con riesgo de enfermedad","Pacientes con y sin diabetes",
                                                           "Paciente consume o no medicamentos","Pacientes con y sin hipertension",
                                                           "Pacientes con y sin infarto previo")) ,
                                   div( uiOutput("imag3"),align="center")
                               )
                       ),
                       #Graficas del análisis exploratorio
                      #el ususario puede elegir la gráfica a mostrar
                       tabItem(tabName = "im4",
                               fluidRow( 
                                   titlePanel(HTML('<h2><center>Población general</h2>')), 
                                   selectInput("ima4", "Seleccione ",
                                               choices = c("Comportamiento nivel de colesterol","Comportamiento masa muscular",
                                                           "Comportamiento presión sistolica","Comportamiento presión diastolica",
                                                           "Comportamiento nivel de glucosa","Comportamiento de frecuencia cardiaca")) ,
                                   div( uiOutput("imag4"),align="center")
                                   )
                               ),
                       #Predicciones con ambos modelos para mujeres
                       #el usuario puede elegir las predicciones del modelo que desee
                       tabItem(tabName = "datam",
                               fluidRow( 
                                   titlePanel(HTML('<h2><center>Mujeres con riesgo</h2>')), 
                                   selectInput("m", "Seleccione ",
                                               choices = c("Mujeres_modelo_logistico","Mujeres_maquina_vectores") ),
                                   div(dataTableOutput("datam"),align="center")
                               )
                               
                       ),
                       #Predicciones con ambos modelos para hombres
                      #el usuario puede elegir las predicciones del modelo que desee
                       tabItem(tabName = "datah",
                               fluidRow( 
                                   titlePanel(HTML('<h2><center>Hombres con riesgo</h2>')), 
                                   selectInput("h", "Seleccione ",
                                               choices = c("Hombres_modelo_logistico","Hombres_maquina_vectores") ),
                                   div(dataTableOutput("datah"),align="center")
                               )
                               
                       )
                       )
                       )
                       )
        )
    

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    
    library(dplyr)
    library(effects)
    library(e1071)#para maquina de vectores
    library(DescTools) #para la moda
    library(psych)
    library(tidyr)
    library(ResourceSelection)
    library(ggplot2)
    #Carga de datos de las predicciones por modelo y sexo
    Mujeres_modelo_logistico<-read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/Mujeres_modelo_logistico.csv")
    Mujeres_maquina_vectores<-read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/Mujeres_maquina_vectores.csv")    
    Hombres_modelo_logistico<-read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/Hombres_modelo_logistico.csv")
    Hombres_maquina_vectores<-read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/Hombres_maquina_vectores.csv")
    
    #transformación de variables de entrada
    m <- reactive({
        w <- get(input$m)
    })
    
    h <- reactive({
        d <- get(input$h)
    })
 
    #cargamos los datos directo del repositorio y los limpiamos.
    train= read.csv("https://raw.githubusercontent.com/angyf/Proyecto-equipo-10/main/train.csv")
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
    train<-mutate(train,sex=as.character(train$sex))
    levels(train$sex) <- c("Mujer", "Hombre")
    #Elegimos solamente algunas variables que se van a analizar
    train<-mutate(train,Frecuencia_cardiaca=heartRate,Nivel_de_colesterol=totChol,
                  Nivel_de_glucosa=glucose,Presion_sistolica=sysBP)
    
    #boxplot que variara dependiendo de que información quiera el usuario
    colores.box<-c("pink","lightblue")
    output$plot <- renderPlot({
        x <- train[,input$x]
        
        ggplot(train,aes(sex,x),y=x)+
            geom_boxplot(color = "black", fill =colores.box)+
            labs(x="Sexo",y=input$x)+
            theme_minimal()+
            theme(plot.title = element_text(hjust = 0.5))+
            scale_x_discrete(labels = c('Mujer','Hombre'))
        
    })   
    
    #Data Table de los datos
    output$data_table <- renderDataTable( {train}, 
                                          options = list(aLengthMenu = c(10,20,50),
                                                         iDisplayLength = 10)
    )
    
    #Data Table de las mujeres que fueron predichas con riesgo de enfermedad
    output$datam <- renderDataTable( {m<-m()},options= list(dom = 'ft'))
    
    
    #Data Table de los hombres que fueron predichos con riesgo de enfermedad
    output$datah <- renderDataTable( {h<-h()},options= list(dom = 'ft'))
    
    
    #Imagenes pestaña 1 
    #el if se puso para elegir la imagen correcta de acuerdo a la que escoja el usuario
    output$imag1 <- renderUI({
        if(input$ima1 == "Población de referencia"){            
            img( src = "Poblacion de referencia.png")
        }                                        
        else if(input$ima1 == "Fumadores y no fumadores"){
            img( src = "Fumadores y no fumadores.png")
        }
        else if(input$ima1 == "Hipertensos y no hipertensos"){
            img( src = "Hipertensos y no hipertensos.png")
        }
        else if(input$ima1 == "Diabeticos y no diabeticos"){
            img(src = "Diabeticos y no diabeticos.png")
        }
        else if(input$ima1 == "Pacientes con y sin riesgo"){
            img( src = "Pacientes con y sin riesgo.png")
        }
    })
    #Imagenes pestaña 2
    #el if se puso para elegir la imagen correcta de acuerdo a la que escoja el usuario
    output$imag2 <- renderUI({
        if(input$ima2 == "Con y sin diabetes"){            
            img( src = "Con y sin diabetes.png")
        }                                        
        else if(input$ima2 == "Con y sin riesgo"){
            img( src = "Con y sin riesgo.png")
        }
        else if(input$ima2 == "Con y sin hipertension"){
            img( src = "Con y sin hipertension.png")
        }
       })
   
    #Imagenes pestaña 3
    #el if se puso para elegir la imagen correcta de acuerdo a la que escoja el usuario
    output$imag3 <- renderUI({
        if(input$ima3 == "Con riesgo de enfermedad"){            
            img( src = "Con riesgo de enfermedad.png")
        }                                        
        else if(input$ima3 == "Pacientes con y sin diabetes"){
            img( src = "Pacientes con y sin diabetes.png")
        }
        else if(input$ima3 == "Paciente consume o no medicamentos"){
            img( src = "Paciente consume o no medicamentos.png")
        }
        else if(input$ima3 == "Pacientes con y sin hipertension"){
            img(src = "Pacientes con y sin hipertension.png")
        }
        else if(input$ima3 == "Pacientes con y sin infarto previo"){
            img( src = "Pacientes con y sin infarto previo.png")
        }
    })
    
    #Imagenes pestaña 4
    #el if se puso para elegir la imagen correcta de acuerdo a la que escoja el usuario
    output$imag4 <- renderUI({
        if(input$ima4 == "Comportamiento presión diastolica"){            
            img( src = "Comportamiento presion diastolica.png")
        }                                        
        else if(input$ima4 == "Comportamiento presión sistolica"){
            img( src = "Comportamiento presion sistolica.png")
        }
        else if(input$ima4 == "Comportamiento masa muscular"){
            img( src = "Comportamiento masa muscular.png")
        }
        else if(input$ima4 == "Comportamiento nivel de glucosa"){
            img(src = "Comportamiento nivel de glucosa.png")
        }
        else if(input$ima4 == "Comportamiento nivel de colesterol"){
            img( src = "Comportamiento nivel colesterol.png")
        }
        else if(input$ima4 == "Comportamiento de frecuencia cardiaca"){
            img( src = "Comportamiento de frecuencia cardiaca.png")
        }
    })
}

shinyApp(ui, server)
