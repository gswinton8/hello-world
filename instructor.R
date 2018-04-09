library(shiny)
library(grid)
library(gridExtra)

nteams <- 3

ui <- fluidPage(
  headerPanel("Create Your Challenge!"),
  mainPanel(
    selectInput("separate", "How would you like to upload your data?",
                c("Test/training data separately" = "yes",
                  "One file including everything" = "no")),
    uiOutput("upload"),
    selectInput("loss", "Specify your loss function", 
                choices = c("Misclassification rate" , "MSE"),
                selected = "Misclassification rate"),
    fileInput('roster', 'Upload the roster',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    numericInput("nsub", "How many predictions can each team submit?",
                 value = 3, min = 1),
    actionButton("create", "Create Challenge!"),
    strong(textOutput('created')),
    br(),
    strong(textOutput('downloadkeymsg')),
    uiOutput('downloadkey'),
    br(),
    uiOutput('downloadpanel')
  )
) 

server <- function(input, output){
  output$upload <- renderUI({
    if(input$separate == "yes") {
      return(
        wellPanel(
          fileInput('trdata', 'Upload your training set',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
          fileInput('testdata', 'Upload your test set',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
          tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator', 
                       c(Comma=',',Semicolon=';',Tab='\t'),',')
        )
      )
    } 
    if (input$separate == "no") {
      return(
        wellPanel(
          fileInput('data', 'Upload your entire dataset',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
          tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator', 
                       c(Comma=',',Semicolon=';',Tab='\t'),','),
          numericInput('trprop', "What proportion of the entire sample should 
                       the training set include?", value = .7, min = 0, max = 1) 
        ) 
      )
    }
  })
  
  
  # data is a list contatining two elements: train and test
  data <- reactive({ 
    if(input$separate == "yes" & length(input$trdata)!=0 
       & length(input$testdata)!=0){
      train <- read.csv((input$trdata)$datapath, header=input$header, 
                        sep=input$sep)
      test <- read.csv((input$testdata)$datapath, header=input$header, 
                       sep=input$sep)
      return(list(train = train, test = test)) 
    } else if(input$separate == "no" & length(input$data != 0)){
      data <- read.csv((input$data)$datapath, header=input$header, 
                       sep=input$sep)
      N <- nrow(data)
      trainind <- sort(sample(1:N, round(input$trprop*N)))
      train <- data[trainind,]
      test <- data[-trainind,]
      return(list(train = train, test = test))
    } else {
      return(NULL)
    }
  })
  
  roster <- reactive({
    unlist(read.csv((input$roster)$datapath, header = FALSE, as.is = TRUE))
  })
  
  observeEvent(input$create, {
    # We should change the directory structure later on
    dir.create(file.path("..","studentapp"))
    temp <- readLines("studentTemplate.R")
    temp <- gsub("XXXlossXXX", input$loss, temp)
    temp <- gsub("XXXnsubXXX", input$nsub, temp)
    cat(temp, file=file.path("..","studentapp","studentapp.R"), sep="\n")
    write.csv(data()$train, file.path("..","studentapp","train.csv"), 
              row.names = FALSE)
    write.csv(data()$test[,-1], file.path("..","studentapp","test.csv"), 
              row.names = FALSE)
    write.csv(data()$test[,1],file.path("..","studentapp","labels.csv"), 
              row.names = FALSE)
    file.copy((input$roster)$datapath, overwrite = TRUE,
              file.path("..","studentapp","roster.csv"))
    
    # Create submission keys
    nteams <-reactive({ length(roster()) })
    # dir.create(file.path("..","studentapp","keys"))
    keys <- matrix(sample(1:(10*nteams()*input$nsub), nteams()*input$nsub),
                   nrow = nteams())
    keys <- data.frame(keys)
    # Change next line so that the instructor can upload the roster 
    rownames(keys) <- roster()
    colnames(keys) <- paste0("key", 1:input$nsub)
    # Add some lines to allow the instructor to download the submission keys
    # Maybe not here, but somewhere
    pdf(file.path("..","studentapp","allkeys.pdf"))
    for (team in roster()){
      write.csv(keys[team,], 
                file.path("..","studentapp",paste0(team,".csv")),
                row.names = FALSE)
      grid.table(keys[team,]) 
      if(length(roster()) != which(team==roster())) { grid.newpage() }
    }
    dev.off()
    
    output$created <- renderText({ 
      "Challenge & Keys Created!"
    })
    output$downloadkeymsg <- renderText({ 
      "You Can Download The Keys Now!"
    })
    
    output$keybutton <- downloadHandler(
      filename = function(){ "allkeys.pdf" },
      content = function(file) {
        file.copy(file.path("..","studentapp","allkeys.pdf"), file)
      }
    )
    
    output$downloadkey <- renderUI({
      return(downloadButton('keybutton', 'Download Key Files'))
    })
    
    # Here we allow the instructor to download the data they previously uploaded
    
    output$downloadpanel <- renderUI({
      return(
        wellPanel(
          strong("Download your data!"),
          selectInput("dataset", "Choose a dataset:", 
                      choices = c("Test", "Training")),
          radioButtons("filetype", "File type:",
                       choices = c("csv", "tsv")),
          downloadButton('downloadData', 'Download')
        )
      )
    })
    
    # Fetches the correponding dataset
    datasetInput <- reactive({
      switch(input$dataset,
             "Test" = data()$test,
             "Training" = data()$train)
    })
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadData <- downloadHandler(
      
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste(input$dataset, input$filetype, sep = ".")
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
        
        # Write to a file specified by the 'file' argument
        write.table(datasetInput(), file, sep = sep,
                    row.names = FALSE)
      }
    )
    
  })
}

shinyApp(server = server, ui = ui)

