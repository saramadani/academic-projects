library(shiny)
library(devtools)
ronfle<-read.table("/home/saara/Desktop/shiny_ronfle/ronfle.txt", header = TRUE)
View(ronfle)
# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    summary(ronfle)
  })
  
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed. 
  output$view <- renderTable({
    set.seed(62433)
    ronfle[sample(nrow(ronfle), size = input$obs),]
  })
  output$hist_univariee = renderPlot({
    
    quali = ronfle[,input$vquali]
    quanti = ronfle[,input$vquanti]
    if(input$type == "Qualitative"){
      barplot(table(quali),xlab = names(quali),ylab = "Frequence",col = "#CC0066",main= paste("Plot de ",names(quali)))
      #plot_ly(y = ~table(quali) , type = "bar" , line = list(color = "yellow"))%>%layout(title = paste("Plot de ",names(quali)))
    }
    if(input$type == 'Quantitative'){
      boxplot(quanti,xlab = names(quanti),ylab = "Frequence",col = "#3399FF",border = "#cc0099",main = paste("Boxplot de ",names(quanti)))
      #plot_ly(y = ~unlist(quanti) , type = "box" , line = list(color = "green"))%>%layout(title = paste("Boxplot de ",names(quanti)))
    }
    
  }
  )
  
  output$plot_univariee = renderPlot({
    
    quali = ronfle[,input$vquali]
    quanti = ronfle[,input$vquanti]
    if(input$type == "Qualitative"){
      plot(density(unlist(quali)), na.rm = TRUE, main = paste("Densite de ",names(quali)))
    }
    if(input$type == 'Quantitative'){
      plot(density(unlist(quanti), na.rm = TRUE), main = paste("Densite de ",names(quanti)))
      
    }
    
  }
  )
  
  output$desc_univariee = renderText({
    
    quali = ronfle[,input$vquali]
    quanti = ronfle[,input$vquanti]
    if(input$type == "Qualitative"){
      summary(quali)
    }
    else if(input$type == 'Quantitative'){
      summary(quanti)
      
    }
    
  }
  )
  output$box_bivariee = renderPlot({
    quali1 = ronfle[,input$vquali1]
    quanti1 = ronfle[,input$vquanti1]
    if(input$type1 == "Qualitative"){
      y = unlist(quali1)
      boxplot(y~RONFLE,xlab = "RONFLE",ylab = names(quali1),names=c("yes","no"),col = "#3399FF",border = "#cc0099",main = paste("Boxplot de ","RONFLE~",names(quali1)))
      #mosaicplot(RONFLE ~ y,xlab = "Good",ylab = names(quali1),shade = TRUE, main = "Graphe en mosaique")
    }
    if(input$type1 == "Quantitative"){
      x = unlist(quanti1)
      boxplot(x~RONFLE,xlab ="RONFLE",ylab = names(quanti1),col = "pink",border = "grey",main = paste("Boxplot de ","RONFLE~",names(quanti1)))
      
    }
    
  }
  )
  
  output$mos_bivariee = renderPlot({
    quali1 = ronfle[,input$vquali1]
    if(input$type1 == "Qualitative"){
      y = unlist(quali1)
      mosaicplot(RONFLE ~ y,xlab = "RONFLE",ylab = names(quali1),shade = TRUE, main = "Graphe en mosaique")
    }
  }
  )
  
  output$bar_bivariee = renderPlot({
    quali1 = ronfle[,input$vquali1]
    if(input$type1 == "Qualitative"){
      y = unlist(quali1)
      bartable = table(RONFLE,y)
      barplot(bartable, col = c("tomato","lightskyblue") ,legend = c("Yes","No"), main = paste(names(quali1),"Selon RONFLE"))
    }
  }
  )
  
  output$test_simple = renderTable({
    
    quanti1 = ronfle[,input$vquanti1]
    if(input$type1 == 'Quantitative'){
      
      x1 = unlist(quanti1)
      kt = as.data.frame(unlist(t.test(x1~RONFLE)))
      
    }
    
  }, rownames = T,colnames = F
  )
  
  output$res_bi = renderTable({
    if(input$type1 == "Qualitative"){
      quali1 = ronfle[,input$vquali1]
      y = unlist(quali1)
      bartable = table(RONFLE,y)
      as.data.frame(lprop(bartable, digits = 2, percent = TRUE))
    }
  }
  )
  
  
  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #    (it only executes a single time)
  #
  algorithmInput <- reactive(input$algorithm)
  
  output$results <- renderPrint({
    
    
    # split in training testing datasets
    trainIndex <- sample(nrow(ronfle), size = nrow(ronfle)*input$slidertrainsplit)
    training = ronfle[trainIndex,]
    testing = ronfle[-trainIndex,]
    
    # apply selected classification algorithm
    if(algorithmInput()=="rpart") {
      
      # print classification parameters
      print("Algorithm selected: rpart")
      print(paste("Training set: ", input$slidertrainsplit*100, "%", sep = ""))
      print(paste("Testing set: ", (1-input$slidertrainsplit)*100, "%", sep = ""))
      
      # build rpart model
      library(rpart)
      set.seed(62433)
      model <- rpart(RONFLE ~ . , data= ronfle)
      
      # test rpart model
      pred <- predict(model, testing, type  = "class")
      print(table(predicted = pred, reference = testing$RONFLE))
      
      # print model
      summary(model)
      
    } else if(algorithmInput()=="randomForest") {
      
      # print classification parameters
      print("Algorithm selected: randomForest")
      print(paste("Training set: ", input$slidertrainsplit*100, "%", sep = ""))
      print(paste("Testing set: ", (1-input$slidertrainsplit)*100, "%", sep = ""))
      
      # build randomForest model
      library(randomForest)
      set.seed(62433)
      model <- randomForest(RONFLE ~ . , data= ronfle)
      
      # test randomForest model
      pred <- predict(model, testing, type  = "class")
      print(table(predicted = pred, reference = testing$RONFLE))
      
      # print model
      summary(model)
      
      
    } else if(algorithmInput()=="lda") {
      
      
      # print classification parameters
      print("Algorithm selected: lda")
      print(paste("Training set: ", input$slidertrainsplit*100, "%", sep = ""))
      print(paste("Testing set: ", (1-input$slidertrainsplit)*100, "%", sep = ""))
      
      # build lda model
      library(MASS)
      set.seed(62433)
      model <- lda(RONFLE ~ . , data= ronfle)
      
      # test lda model
      pred <- predict(model, testing, type  = "class")
      print(table(predicted = pred$class, reference = testing$RONFLE))
      
      # print model
      summary(model)      
      
    }  else{
      print("Error no Algorithm selected")
    }
    
  }) 
  
})
