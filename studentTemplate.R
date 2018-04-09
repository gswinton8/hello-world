library(shiny)

setwd("../studentapp")
dir.create("predictions")
dir.create("scores")
labels <- read.csv("labels.csv", header = TRUE)
roster <- unlist(read.csv("roster.csv", as.is = TRUE, header = FALSE))
                 
names(roster) <- NULL
loss <- "XXXlossXXX"
nsub <- XXXnsubXXX

ui <- fluidPage(
  titlePanel("Welcome to student's app!"),
  
  tabsetPanel(
    tabPanel("Download the data",
             selectInput("dataset", "Choose a dataset:", 
                         choices = c("Train", "Test")),
             downloadButton('downloadData', 'Download')
    ),
    tabPanel("Upload your results",
             selectInput("team", "Team?",
                         choices = roster),
             fileInput('pred', 
                       'Choose CSV File (do NOT include column names)',
                       accept=c('csv', 
                                'comma-separated-values', 
                                '.csv')),
             numericInput("key", "Enter a valid submission key", value = 0),
             actionButton("submit", "Submit!"),
             textOutput("keyvalid")
    ),
    tabPanel("Leaderboard", tableOutput('leaderboard'))
  )
)

server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Train" = "train.csv",
           "Test" = "test.csv")
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){ paste0(input$dataset, '.csv') },
    content = function(file) {
      file.copy(datasetInput(), file)
    }
  )
  
  observeEvent(input$submit, { 
    key <- reactive({ 
      unlist(read.csv(paste0(input$team,".csv"), header = TRUE))
    }) 
    if(!(input$key %in% key())){
      output$keyvalid <- renderText({"Invalid submission key"})
    } else{
      
      output$keyvalid <- renderText({
        "Your prediction was successfully submitted!"
      })
      
      subno <- nsub-length(key())+1 
      write.csv(key()[!(key()==input$key)],
                paste0(input$team,".csv"),
                row.names = FALSE)
      
      studentPred <- reactive({
        file.copy((input$pred)$datapath, 
                  paste0("predictions/pred_",input$team,"_",subno,".csv"),
                  overwrite = TRUE)
        read.csv((input$pred)$datapath, header = FALSE, as.is = TRUE)
      })
      
      if (loss == "MSE") {
        error <- reactive({
          sum((labels-studentPred())^2)/nrow(labels)
        })  
      }
      
      if (loss == "Misclassification rate") {
        error <- reactive({
          sum(labels != studentPred())/nrow(labels)
        })
      }
      
      write.csv(data.frame(input$team, error()), row.names = FALSE,
                file = paste0("scores/",input$team,"_",subno,".csv"))
      
      output$leaderboard <- renderTable({ 
        scores <- dir("scores")
        
        if(length(scores) != 0){
          board <- data.frame(Name = rep(NA,length(scores)), Score = NA) 
          for (i in 1:length(scores)){
            board[i,] <- read.csv(paste0("scores/",scores[i]),
                                  header = TRUE, as.is = TRUE)
          }
          board <- cbind(data.frame(Rank = 1:length(scores)),
                         board[order(board[,2]),])
          return(board)
        } else{
          return(NULL)  
        }
      })  
    }
  }) 
}


shinyApp(ui = ui, server = server)

