library(shiny)
attach(ronfle)
# Define UI for dataset viewer application
shinyUI(fluidPage(
  navbarPage(
    
    "Machine Learning Project",
    ## L'Analyse Univariee
    tabPanel("Data",
             
             
             # Sidebar with controls for the algorithms and output
             sidebarLayout(
               position = "left",
               
               sidebarPanel(
                 
                 numericInput("obs", "Number of observations to view:", 6)), 
               
               
               
               # Show the some example of observations, a summary of the dataset 
               # and the results on the model
               mainPanel(
                 
                 p("This application allows us to evaluate three different classification algorithms (rpart, randomForest and lda) to the ronfle dataset."),
                 p("First it shows randomly n observations of the ronfle dataset, where n is the number of observations specified on the sidebar panel (6 by default)."),
                 p("Then it shows summary statistics for the ronfle dataset."),
                 p("Lastly it trains a classification algorithm chosen by the user on the sidepar panel, splitting the ronfle dataset into training and testing sets based on the chosen proportion chosen on the sidebar (by defuault 70% training 30% testing)."),
                 
                 
                 
                 h2("Dataset"),
                 tableOutput("view"), 
                 
                 h2("Statistics Summary"),
                 verbatimTextOutput("summary")
                 
               )
             )
    ),
    tabPanel("Univaried Analysis",
             sidebarPanel(
               selectInput("type","Select the type of the variable",
                           choices = c("Quantitative","Qualitative")),
               conditionalPanel( condition = "input.type == 'Quantitative'",
                                 selectInput("vquanti","Choose the variable",
                                             choices = c(colnames(ronfle[c(1,2,3,4)])) 
                                 )
               ),
               conditionalPanel( condition = "input.type == 'Qualitative'",
                                 selectInput("vquali","Chooose the variable",
                                             choices = c(colnames(ronfle[-c(1,2,3,4)])) 
                                 ))
             ),
             mainPanel(
               h4("Statistics Metrics "),
               hr(),
               h4(verbatimTextOutput("desc_univariee")),
               hr(),
               h4("Graphs"),
               hr(),
               plotOutput("hist_univariee"),
               hr(),
               plotOutput("plot_univariee")
             )
    ),
    tabPanel("Bi-varied Analysis", 
             sidebarPanel(
               selectInput("type1","Select the type of the variable",
                           choices = c("Quantitative","Qualitative")),
               conditionalPanel( condition = "input.type1 == 'Quantitative'",
                                 selectInput("vquanti1","Choose the variable ~ RONFLE",
                                             choices = c(colnames(ronfle[c(1,2,3,4)])) 
                                 )
               ),
               conditionalPanel( condition = "input.type1 == 'Qualitative'",
                                 selectInput("vquali1","Choose the variable ~ RONFLE",
                                             choices = c(colnames(ronfle[-c(1,2,3,4)])) 
                                 ))
             ),
             mainPanel(
               plotOutput("box_bivariee"),
               hr(),
               h4(tableOutput("test_simple")),
               plotOutput("mos_bivariee"),
               hr(),
               plotOutput("bar_bivariee"),
               h4(tableOutput("res_bi"))
             )
    ),
    tabPanel("Testing Classification Algorithms ",
             
             
             # Sidebar with controls for the algorithms and output
             sidebarLayout(
               position = "left",
               
               
               sidebarPanel (      
                 sliderInput("slidertrainsplit",
                             "Proportion of Training observations",
                             min = 0, max = 1, value = 0.7, step = 0.1),
                 
                 selectInput("algorithm", "Choose a Classification algorithm:", 
                             choices = c("rpart", "randomForest", "lda"))
                 
                 
               ),
               
               
               # Show the some example of observations, a summary of the dataset 
               # and the results on the model
               mainPanel(
                 
                 p("This application allows us to evaluate three different classification algorithms (rpart, randomForest and lda) to the ronfle dataset."),
                 p("First it shows randomly n observations of the ronfle dataset, where n is the number of observations specified on the sidebar panel (6 by default)."),
                 p("Then it shows summary statistics for the ronfle dataset."),
                 p("Lastly it trains a classification algorithm chosen by the user on the sidepar panel, splitting the ronfle dataset into training and testing sets based on the chosen proportion chosen on the sidebar (by defuault 70% training 30% testing)."),
                 
                 
                 
                 
                 
                 h2("Results"),
                 verbatimTextOutput("results")
               )
             )
    )
    
    
    
    
    
    
    
    
  )
  
  
  
  
))