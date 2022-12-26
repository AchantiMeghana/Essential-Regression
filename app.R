
library(shiny)
library(shinyWidgets)
library(rmarkdown)
library(readr)

# Define UI for application
ui <- fluidPage(
    titlePanel("Essential Regression"),
    
    fluidRow(
      column(6,
             #Uploading data set
             fileInput("upload","Upload your dataset"),
             
             #Choose independent variables
             pickerInput("x",label ="Choose the independent variables",
                         choices = NULL,selected= NULL,multiple = TRUE),
             br(),
             
             #Choose dependent variable
             pickerInput("y",label ="Choose the dependent variable",
                         choices = NULL,selected= NULL,multiple=FALSE),
             br(),
      
             #Choose delta
             numericInput("delta",label ="Choose delta",
                          value = NULL),
             br(),
            
             #Choose lambda
             numericInput("lambda",label ="Choose lambda",
                          value = NULL),
             br(),
            
             #Choose beta_est_method
             pickerInput("beta_est",
                         label ="Choose the method to be used to estimate beta", 
                         choices = NULL,selected= NULL),
            
             #Action button
            actionButton("submit","Submit the choices of parameters")
            ),
      
      column(6,
             #Add an image
             img(src = "Regression_cartoon.jpg",width =500)
             )
    ),
    fluidRow(
      column(6,
             #Display data set
             tableOutput("data_table"),
             
              #Download report
             downloadButton("download",label = "Get your report!")
      )
    ),
    
    fluidRow(
      column(6,
             #Display beta
             h3("beta"),
             textOutput("beta")
             )
    )
    
)
              
  

# Define server logic 
server <- function(input, output,session) {
  
  #Read data
  data <- reactive({
    req(input$upload)
    data <- read_delim(input$upload$datapath, " ",
                               escape_double = FALSE, trim_ws = TRUE)})
  
  #Update y widget
  observeEvent(data(),{
    updatePickerInput(session=session,inputId = "y",choices = colnames(data()))
  })
 
  #Update x widget
  observeEvent(data(),{
    updatePickerInput(session=session,inputId = "x",choices = colnames(data()))
  })
  
  #Update beta_est_method
  observeEvent(data(),{
    updatePickerInput(session=session,inputId ="beta_est",
                      choices = c("Dantzig","LS"))
  })
  
  #Display data set
  output$data_table <- renderTable(head(data()))

  
 
  ####Essential Regression using LS
  
  x_col <- reactive({
    req(data())
    colnames(data())
  })
  
  x_indices <- reactive({
    req(data())
  
     #Returns matrix of indices of chosen columns    
    match(input$x,x_col())
    })
 
   #Obtain x_data
  x_dat <- reactive({
    req(input$submit)
    data()[,as.numeric(unlist(x_indices()))]
    })
  
  feature_names <- reactive(names(x_dat()))
  
  y_dat <- reactive({
    req(input$submit)
    data()[,input$y]
    })
  
  Y <- reactive( {y_dat() - colMeans(data()[,input$y],na.rm = TRUE)})
  X <- reactive({scale(x_dat(),center = TRUE,scale = TRUE )})
  
  
  source("SupLOVE.R")
  source("K-CV.R")
  source("Helper.R")
  source("Other_algorithms.R")
  
  res <- reactive({
          req(input$submit,data())
            ER(Y(), X(), delta = input$delta, pred = F, beta_est = input$beta_est, 
            rep_CV = 50, verbose = T,
            merge = F, CI = T, correction = NULL, lbd = input$lambda)
    
  })
  
 
  #Obtain beta, beta_scaled,CIs,p-vals
  beta <- reactive({
    res()$beta})
  output$beta <- renderText(beta())
  
  beta_scaled <- reactive({
    diag(sqrt(diag(res()$C))) %*% beta()})
  
  CIs <- reactive({
    round(res()$beta_CIs, 3)})
 
   p_vals <- reactive({
     2 * pnorm(abs(res()$beta/ sqrt(res()$beta_var / length(Y()))), lower.tail = F)
     })
  
  output$download <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      
      #Using a temporary directory
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Setting up parameters to pass to Rmd document
      params <- list(isolate(beta()),
                     isolate(beta_scaled()),
                     isolate(CIs()),
                     isolate(p_vals()),
                     isolate(input$beta_est))

      #Knitting the document
      rmarkdown::render(tempReport, output_file = file,
                        params = params)}
    
    
  )
  
   #output$beta <- renderText({
   # paste0("Typeof x_dat is " ,typeof(x_dat()),
   #        ".Type of X is ",typeof(X()),
    #       ".the value of beta is",res()$beta)
    
  }

     
      


# Run the application 
shinyApp(ui = ui, server = server)
