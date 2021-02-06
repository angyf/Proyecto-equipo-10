
library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)

#Esta parte es el análogo al ui.R
ui <- 
    
    fluidPage(
      
        
        dashboardPage( skin = "red",
            
            dashboardHeader(title = "Proyecto"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Boxplot", tabName = "boxp", icon = icon("bar-chart")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table"))
                    )
                
            ),
            
            dashboardBody(
              tags$head(
                
                tags$style(HTML(".body { 
                magin:auto;
                background-color: white;
                color: black;
      }
      "))
              ),
                
                tabItems(
                    # Boxplot
                    tabItem(tabName = "boxp",
                            fluidRow( 
                                titlePanel(HTML('<h2><center>Boxplot</h2>')), 
                                selectInput("x", "Seleccione ",
                                            choices = c("Frecuencia_cardiaca","Nivel_de_colesterol", "Nivel_de_glucosa","Presion_sistolica") ),
                              div(plotOutput("plot", height = 450, width = 600),align="center")
                            )
                                
                            
                    ),
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(HTML('<h2><center>Data table</h2>')),
                                dataTableOutput ("data_table")
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
  
     train= read.csv("https://storage.googleapis.com/kagglesdsdata/datasets/888068/1507687/train.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210204%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210204T014641Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=35fcabbaaff8dc551e1aac9950c61cb46c338770f2347772dc067d951ca811a0e4aed71d7529a93479d6e4cbb23fa9febcf3216e329893bcf10cb8657270d4f54ed41974b76aec35c61ab64523acba6215625ebf92b5147f322d3bbbda97c72dd4bec47e05d28a728ed284a9a15db13b5472ee464d9fea646461121f0635f1d7c8d59bcc6bba921466bc420fece6f64a41d600a12e74eb4901d0e1612eabda146386079ac65d3f7ae37bcb99e7cb341f90c3e5875154592ad5729792dbee911e30d6e809c3796f4b0b0ab3aa27bfd431eea6af1a877647083d26a1135e15b5506a4941d360c6d2fbf2d185a3040666eb4295e5908a8ef2068b34a0a7f7a1f59e")
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
     
     
     train<-mutate(train,Frecuencia_cardiaca=heartRate,Nivel_de_colesterol=totChol,
                   Nivel_de_glucosa=glucose,Presion_sistolica=sysBP)
     
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
    
      #Data Table
      output$data_table <- renderDataTable( {train}, 
                                            options = list(aLengthMenu = c(10,20,50),
                                                           iDisplayLength = 10)
      )
 }


shinyApp(ui, server)
