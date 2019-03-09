tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("cerulean"),
    "Machine Learning Project",
    ## L'Analyse Univariee
    tabPanel("Univariate Analysis",
             sidebarPanel(
               selectInput("type","Select the type of the variable",
                           choices = c("Quantitative","Qualitative")),
               conditionalPanel( condition = "input.type == 'Quantitative'",
                                 selectInput("vquanti","Choose the variable",
                                             choices = c(colnames(german_data[,c(2,5,13)])) 
                                 )
               ),
               conditionalPanel( condition = "input.type == 'Qualitative'",
                                 selectInput("vquali","Choose the variable",
                                             choices = c(colnames(german_data[,-c(2,5,13)])) 
                                 ))
             ),
             mainPanel(
               h4("Statistics Metrics "),
               hr(),
               h4(verbatimTextOutput("desc_univariee")),
               hr(),
               h4("Graph"),
               hr(),
               plotOutput("hist_univariee"),
               hr(),
               plotOutput("plot_univariee")
             )
    ),
  tabPanel("Bivariate Analysis", 
             sidebarPanel(
               selectInput("type1","Select the type of the variable",
                           choices = c("Quantitative","Qualitative")),
               conditionalPanel( condition = "input.type1 == 'Quantitative'",
                                 selectInput("vquanti1","Choose the variable ~Good",
                                             choices = c(colnames(german_data[,c(2,5,13)])) 
                                 )
               ),
               conditionalPanel( condition = "input.type1 == 'Qualitative'",
                                 selectInput("vquali1","Choose the variable ~Good",
                                             choices = c(colnames(german_data[,-c(2,5,13)])) 
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
    tabPanel("Logistic Regression",
             
             mainPanel(
               h4("Model:"),
               hr(),
               verbatimTextOutput("mdl"),
               hr(),
               plotOutput("pred"),
               hr(),
               tableOutput("glm")
               
             ),
             sidebarPanel(
               sliderInput("slider1", label = h3("Choose the size of Train Data"), min = 0, 
                           max = 1000, value = 700),
               checkboxGroupInput("checkGroup", label = h3("Choose the explanatory variables"), 
                                  choices = c(colnames(german_data[,-21])),
                                  selected = c(colnames(german_data[,c(1,2,3)]))
               )
             )
             
    ),
    tabPanel(
      "Machine Learning",
      mainPanel(
        h4("Model"),
        hr(),
        verbatimTextOutput("mdl2"),
        hr(),
        conditionalPanel(
          condition = "input.meth == 'Neural Networks (nnet)'",
          plotOutput("plot_rn"),
          hr(),
          plotOutput("neural", width = 1200 , height = 1500)
        ),
        conditionalPanel(
          condition = "input.meth == 'Decision tree'",
          plotOutput("arbre"),
          hr(),
          plotOutput("arbre2", width = 1200 , height = 1500)
        )
      ),
      sidebarPanel(
        selectInput("meth","Select the method",
                    choices = c("Decision tree","Neural Networks (nnet)")),
        sliderInput("slider2", label = h3("Choose the size of Train Data"), min = 0, 
                    max = 1000, value = 700),
        checkboxGroupInput("checkGroup2", label = h3("Choose the explanatory variables"), 
                           choices = c(colnames(german_data[,-21])),
                           selected = c(colnames(german_data[,-21]))
        )
      )
    ),
    
    tabPanel(
      "Comparative Performance",
      mainPanel(
        h1("Comparative Performance for 70% of Train Data"),
        hr(),
        plotOutput("comparaison"),
        hr(),
        tableOutput("tab_comp")
      )
    )
    
  ))