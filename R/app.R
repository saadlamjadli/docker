library(shiny)
library(data.table)
library(randomForest)
library(RMySQL)


# Training set
over <- read.csv("over.csv", header = TRUE)

ui <- fluidPage(
  # Page header
  headerPanel("Prediction du nombre de CD4/ Laboratoire d'immunologie"),
  
  # Input values
  sidebarPanel(
    HTML("<h2>Paramètres d'entrée</h2>"),
    
    selectInput("SEXE", label = "Genre:", 
                choices = list("homme" = "1", "femme" = "2"), 
                selected = "1"),
    sliderInput("AGE", label = "Age : / an",
                min = 18, max = 100,
                value = 18),
    selectInput("STADE", label = "Stade CDC du VIH :", 
                choices = list("A" = "1", "B" = "2", "C"="3"), 
                selected = "1"),
    sliderInput("hb", label = "Taux d'hémoglobine : g/dL",
                min = 4, max = 18,
                value = 4),
    sliderInput("vgm", label = "Volume globulaire moyen: µm3",
                min = 70, max = 120,
                value = 70),
    sliderInput("gr",label =  "Nombre de globules rouge: millions par mm3",
                min = 2, max = 6,
                value = 2),
    sliderInput("plq", label = "Nombre de plaquettes: 10*3 /mm³",
                min = 10, max = 500,
                value = 10),
    sliderInput("ht", "Taux d'hématocrite: en pourcentage",
                min = 30, max = 60,
                value = 30),
    selectInput("tlc11", label = "Nombre de lymphocyte: /mm3 ", 
                choices = list("<1100" = "0", ">=1100" = "1"), 
                selected = "0"),
    
    
    
    actionButton("submitbutton", "Soumettre", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('État/Résultat')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('table')
    
  )
)


over <- read.csv("over.csv", header = TRUE)

# Read in the RF model


model <- readRDS("model.rds")

server <-  shinyServer(function(input, output, session) {
  
  # Input 
  setInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("SEXE",
               "AGE",
               "STADE",
               "hb",
               "vgm",
               "gr",
               "plq",
               "ht",
               "tlc11"),
      Value = as.character(c(input$SEXE,
                             input$AGE,
                             input$STADE,
                             input$hb,
                             input$vgm,
                             input$gr,
                             input$plq,
                             input$ht,
                             input$tlc11)),
      stringsAsFactors = FALSE)
    
    cd4count <- "cd4count"
    df <- rbind(df, cd4count)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test <- rbind(over[1,], test)
    test <- test[-1,]
    test$STADE <- factor(test$STADE, levels  = c("1", "2","3"))
    test$SEXE <- factor(test$SEXE, levels  = c("1", "2"))
    test$cd4count <- factor(test$cd4count, levels  = c("CD4.inf.200", "CD4.sup.ou.égal.200"))
    test$tlc11 <- factor(test$tlc11, levels  = c("0", "1"))

    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calcul terminé.") 
    } else {
      return("Le serveur est prêt pour le calcul.")
    }
  })
  
  # Prediction results table
  output$table <- renderTable({
    if (input$submitbutton>0) { 
      isolate(setInput()) 
    } 
  })
  
})


shinyApp(ui = ui, server = server)
