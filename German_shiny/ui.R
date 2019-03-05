#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Projet Machine Learning"),
  
  tabsetPanel(
    tabPanel("donnees",
             sidebarLayout(
               titlePanel("german credit dataset"),
               mainPanel(dataTableOutput("tab")))
             
             # autre chose
    ),
    tabPanel("analyse un-variee",
             
             
             sidebarLayout(
               sidebarPanel(
                 helpText("la premiere etape consiste a analyser chaque variable "),
                 
                 
                 
                 selectInput("var", 
                             label = "Choisir la variable a analyser",
                             choices = c("Status of existing checking account", 
                                         "Duration",
                                         "Credit history", 
                                         "Purpose",
                                         "Credit amount",
                                         "Savings account/bonds",
                                         "Present employment since",
                                         "Installment rate in percentage of disposable income",
                                         "Personal status and sex",
                                         "Other debtors / guarantors",
                                         "Present residence since",
                                         "Property",
                                         "Age in years",
                                         "Other installment plans",
                                         "Housing",
                                         "Number of existing credits at this bank",
                                         "Job",
                                         "Number of people being liable to provide maintenance for",
                                         "Telephone",
                                         "foreign worker",
                                         "Good"
                                         
                             ),
                             selected = "Status of existing checking account"),
                 
                 conditionalPanel(condition="input.var=='Status of existing checking account'",
                                  radioButtons("status", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 conditionalPanel(condition="input.var=='Duration'",
                                  radioButtons("duration", "Selectionner option",choices =c("summary", "diagramme","boxplot")) ),
                 
                 conditionalPanel(condition="input.var=='Credit history'",
                                  radioButtons("credith", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 conditionalPanel(condition="input.var=='Purpose'",
                                  radioButtons("purpose", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Credit amount'",
                                  radioButtons("credita", "Selectionner option",choices =c("summary", "diagramme","boxplot")) ),
                 conditionalPanel(condition="input.var=='Savings account/bonds'",
                                  radioButtons("savings", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Present employment since'",
                                  radioButtons("present", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 
                 conditionalPanel(condition="input.var=='Installment rate in percentage of disposable income'",
                                  radioButtons("installment", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Personal status and sex'",
                                  radioButtons("personal", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Other debtors / guarantors'",
                                  radioButtons("other", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Present residence since'",
                                  radioButtons("presentr", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Property'",
                                  radioButtons("property", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Age in years'",
                                  radioButtons("age", "Selectionner option",choices =c("summary", "diagramme","boxplot")) ),
                 
                 conditionalPanel(condition="input.var=='Other installment plans'",
                                  radioButtons("otheri", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 
                 conditionalPanel(condition="input.var=='Housing'",
                                  radioButtons("housing", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 
                 conditionalPanel(condition="input.var=='Number of existing credits at this bank'",
                                  radioButtons("number", "Selectionner option",choices =c("summary", "diagramme","boxplot")) ),
                 
                 
                 conditionalPanel(condition="input.var=='Job'",
                                  radioButtons("job", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 conditionalPanel(condition="input.var=='Number of people being liable to provide maintenance for'",
                                  radioButtons("numbero", "Selectionner option",choices =c("summary", "diagramme","boxplot")) ),
                 conditionalPanel(condition="input.var=='Telephone'",
                                  radioButtons("telephone", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 conditionalPanel(condition="input.var=='foreign worker'",
                                  radioButtons("foreign", "Selectionner option",choices =c("summary", "diagramme","camembert")) ),
                 conditionalPanel(condition="input.var=='Good'",
                                  radioButtons("good", "Selectionner option",choices =c("summary", "diagramme","camembert")) )
                 
               ),
               
               mainPanel(verbatimTextOutput("summary"),plotOutput("distPlot")))
    ),
    
    
    
    
    
    tabPanel("analyse bi-variee",
             sidebarLayout(
               sidebarPanel(
                 helpText("la premiere etape consiste a analyser chaque variable "),
                 
                 
                 
                 selectInput("choix", 
                             label = "Choisir la variable a analyser",
                             choices = c("Status of existing checking account", 
                                         "Duration",
                                         "Credit history", 
                                         "Purpose",
                                         "Credit amount",
                                         "Savings account/bonds",
                                         "Present employment since",
                                         "Installment rate in percentage of disposable income",
                                         "Personal status and sex",
                                         "Other debtors / guarantors",
                                         "Present residence since",
                                         "Property",
                                         "Age in years",
                                         "Other installment plans",
                                         "Housing",
                                         "Number of existing credits at this bank",
                                         "Job",
                                         "Number of people being liable to provide maintenance for",
                                         "Telephone",
                                         "foreign worker"
                                         
                                         
                             ),
                             selected = "Status of existing checking account"),
                 
                 conditionalPanel(condition="input.choix=='Status of existing checking account'",
                                  radioButtons("stat", "Selectionner option",choices =c("table", "plot")) ),
                 
                 conditionalPanel(condition="input.choix=='Duration'",
                                  radioButtons("Dur", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Credit history'",
                                  radioButtons("cred", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Purpose'",
                                  radioButtons("purp", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Credit amount'",
                                  radioButtons("creda", "Selectionner option",choices =c("table")) ),
                 conditionalPanel(condition="input.choix=='Savings account/bonds'",
                                  radioButtons("sava", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Present employment since'",
                                  radioButtons("prese", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Installment rate in percentage of disposable income'",
                                  radioButtons("installr", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Personal status and sex'",
                                  radioButtons("persos", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Other debtors / guarantors'",
                                  radioButtons("othrd", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Present residence since'",
                                  radioButtons("presr", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Property'",
                                  radioButtons("proper", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Age in years'",
                                  radioButtons("agei", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Other installment plans'",
                                  radioButtons("othri", "Selectionner option",choices =c("table", "plot")) ),
                 
                 conditionalPanel(condition="input.choix=='Housing'",
                                  radioButtons("hous", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Number of existing credits at this bank'",
                                  radioButtons("numro", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Job'",
                                  radioButtons("jo", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Number of people being liable to provide maintenance for'",
                                  radioButtons("numop", "Selectionner option",choices =c("table", "plot")) ),
                 conditionalPanel(condition="input.choix=='Telephone'",
                                  radioButtons("tele", "Selectionner option",choices =c("table", "plot")) ),
                 
                 conditionalPanel(condition="input.choix=='foreign worker'",
                                  radioButtons("forei", "Selectionner option",choices =c("table", "plot")) )
                 
                 
                 
               ),
               
               mainPanel(verbatimTextOutput("ta"),plotOutput("di")))
    ),
    
    
    
    tabPanel("train",
             
             sidebarLayout(
               sidebarPanel(
                 helpText("la premiere etape consiste a analyser chaque variable "),
                 
                 
                 
                 selectInput("cho", 
                             label = "Choisir la variable a analyser",
                             choices = c("Selection backward", 
                                         "Reseaux de neurones",
                                         "Arbre de decision", 
                                         "Random forest",
                                         "Boosting1",
                                         "Boosting2"
                                         
                                         
                             ),
                             selected = "Selection backward"),
                 conditionalPanel(condition="input.cho=='Reseaux de neurones'",
                                  radioButtons("res", "Selectionner option",choices =c("resume", "plot")) ), 
                 conditionalPanel(condition="input.cho=='Arbre de decision'",
                                  radioButtons("arb", "Selectionner option",choices =c("resume", "plot")) ),
                 conditionalPanel(condition="input.cho=='Random forest'",
                                  radioButtons("ran", "Selectionner option",choices =c("resume", "plot")) ),
                 
                 conditionalPanel(condition="input.cho=='Boosting1'",
                                  radioButtons("boo1", "Selectionner option",choices =c("resume", "plot")) ),
                 conditionalPanel(condition="input.cho=='Boosting2'",
                                  radioButtons("boo2", "Selectionner option",choices =c("resume", "plot")) )
                 
               ),
               
               mainPanel(verbatimTextOutput("trr"),plotOutput("digg")))
    ),
    tabPanel("courbes ",
             tabsetPanel("Panel 1.x",
                         tabPanel("courbes test",plotOutput("dd")),
                         tabPanel("courbes train",plotOutput("gg"))
             )
             
             
    ) ,
    
    tabPanel("ind AUC",
             tabsetPanel("Panel 1.x",
                         tabPanel("regr log",plotOutput("glmfit")),
                         tabPanel("res neur",plotOutput("nnetfit")),
                         tabPanel("arbr dec",plotOutput("rpart")),
                         tabPanel("rand for",plotOutput("rf")),
                         tabPanel("boost1",plotOutput("gbm")),
                         tabPanel("boost2",plotOutput("c50fit"))
                         
             ))
    
    
  ))

)