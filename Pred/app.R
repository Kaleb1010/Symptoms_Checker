library(shiny)

ui <- fluidPage(titlePanel("Demo Interface"),
                
                sidebarLayout(
                  sidebarPanel(
                    textInput("age", "Age:",value = " "),
                    selectInput("sex", "Sex:",
                                c("sex" = "",
                                  "male" = "1",
                                  "female" = "0")),
                    selectInput("cp", "Cp:",
                                c("chest pain" = "",
                                  "typical angina" = "0",
                                  "atypical angina" = "1",
                                  "non-anginal pain" = "2",
                                  "asymptomatic" = "3"  )),
                    textInput("trestbps", "Rbs:",value =" "),
                    textInput("chol", "Serum Cholesteral:",value =" "),
                    selectInput("fbs", "Fbs:",
                                c("fbs > 120mg" = "",
                                  "true" = "1",
                                  "false" = "0")),
                    selectInput("restecg", "Restecg:",
                                c("Rest_ecg" = "",
                                  "normal" = "0",
                                  "having ST-T wave abnormality" = "1",
                                  "showing probable or definite left ventricular hypertrophy by Estes criteria" = "2")),
                    selectInput("thalach", "Thalach:",
                                c("thalach" = "",
                                  "true" = "1",
                                  "false" = "0")),
                    selectInput("exang", "Exang:",
                                c("exercise induced angina" = "",
                                  "yes" = "1",
                                  "no" = "0")),
                    numericInput("oldpeak", "old peak",value = " "),
        
                    selectInput("slope", "Slope:",
                                c("slope" = "",
                                  "unsloping" = "0",
                                  "flat" = "1",
                                  "downsloping" = "2")),
                    
                    selectInput("ca", "Ca:",
                                c("vessels colored by flourosopy" = "",
                                  "1" = "0",
                                  "2" = "1",
                                  "3" = "2",
                                  "4" = "3")),
                    
                    selectInput("thal", "Thal:",
                                c("thal" = "",
                                  "normal" = "1",
                                  "fixed defect" = "2",
                                  "reversable defect" = "3")),
                    
                    actionButton("submit1", strong("Submit"))
                    
                  ),
                  mainPanel(
                    #verbatimTextOutput("age"),
                   # verbatimTextOutput("sex"),
                    tableOutput("inputvals"),
                   textOutput("Pred")
                    
                    
                  )
                )
)


server <- function(input, output, session){
  set.seed(1234)
  observe({  
    #creating object of input gotten
    age <- as.numeric(input$age)
    sex <- as.numeric(input$sex)
    cp <- as.numeric(input$cp)
    trestbps <- as.numeric(input$trestbps)
    chol <- as.numeric(input$chol)
    fbs <- as.numeric(input$fbs)
    restecg <- as.numeric(input$restecg)
    thalach <- as.numeric(input$thalach)
    exang <- as.numeric(input$exang)
    oldpeak <- as.numeric(input$oldpeak)
    slope <- as.numeric(input$slope)
    ca <- as.numeric(input$ca)
    thal <- as.numeric(input$thal)
    
   #action button
    mdata <- eventReactive(input$submit1,{
      dat <- cbind(age,sex,cp,trestbps,chol,fbs,restecg,thalach,exang,oldpeak,slope,ca,thal)
      dat <- as.data.frame(dat)
      
    })
    
  
  # mdata <- eventReactive(input$submit1,{
  # runif(input$age,input$sex,input$cp,input$rbs,input$chol,input$fbs,input$rest_ecg,input$thalach,input$exang,input$old_peak,input$slope,input$ca,input$thal)
   # var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,var13
     
    #input$age,input$sex,input$cp,input$rbs,input$chol,input$fbs,input$rest_ecg,input$thalach,input$exang,input$old_peak,input$slope,input$ca,input$thal)
    # comp = as.data.frame(
    #   var1 = as.numeric(input$age),
    #   var2 = as.character.factor(input$sex),
    #   var3 = as.character.factor(input$cp),
    #   var4 = as.numeric(input$rbs),
    #   var5 = as.numeric(input$chol),
    #   var6 = as.character.factor(input$fbs),
    #   var7 = as.character.factor(input$rest_ecg),
    #   var8 = as.character.factor(input$thalach),
    #   var9 = as.character.factor(input$exang),
    #   var10 = as.numeric(input$old_peak),
    #   var11 = as.character.factor(input$slope),
    #   var12 = as.character.factor(input$ca),
    #   var13 = as.character.factor(input$thal)
    # )
    # newLine <- c(input$age, input$sex, input$cp)
    # values$DT <- rbind(values$DT, newLine)
    
  
    
  # })
  
  #output$age <- renderText({paste("state name is as :", mdata()[1])})
  #output$sex <-renderText({paste("revenue is as :",  mdata()[2])})
  output$inputvals<-renderTable({
   mdata()
  })
  
  pred <- reactive({
    predict(model,newdata = mdata())
  })
  if(pred() == 0){
     res = paste("you do not have a heart disease",pred())
    
  output$Pred <- renderPrint(res)
  }else{
    des =  paste("you have a heart disease",pred())
    output$Pred <- renderPrint(des)
    
  }
  })
 
}






shinyApp(ui, server)