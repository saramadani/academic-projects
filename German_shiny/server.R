#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
  
  output$tab <- renderDataTable({
    
    for(i in 1:1000){
      (if(tab[i,21]=="1") (tab[i,21]=0))  
      (if(tab[i,21]=="2") (tab[i,21]=1)) 
    }
    tab })
  
  library(ggplot2) 
  library(scales) 
  #statut...
  
  y1=data.frame(Status_of_existing_checking_account=1000)
  for (i in 1:1000) { 
    
    (if(tab[i,1]=="1") (y1[i,1]="< 0 DM"))  
    (if(tab[i,1]=="2") (y1[i,1]="0 <= ... <200 DM")) 
    (if(tab[i,1]=="3") (y1[i,1]="... >= 200 DM "))  
    (if(tab[i,1]=="4") (y1[i,1]="no checking account"))  
  } 
  
  
  
  
  library(questionr)
  y3=data.frame(Credit_history=1000)
  for (i in 1:1000) { 
    
    (if(tab[i,3]=="0") (y3[i,1]="no credits taken/all credits paid back duly"))  
    (if(tab[i,3]=="1") (y3[i,1]="all credits at this bank paid back duly")) 
    (if(tab[i,3]=="2") (y3[i,1]="existing credits paid back duly till now")) 
    (if(tab[i,3]=="3") (y3[i,1]="delay in paying off in the past"))  
    (if(tab[i,3]=="4") (y3[i,1]="critical account/other credits existing (not at this bank) "))  
    
  } 
  mycol<-c(  "red", "black","blue","green","magenta")
  y4=data.frame(Purpose=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,4]=="0") (y4[i,1]="car (new)"))  
    (if(tab[i,4]=="1") (y4[i,1]="car (used)"))  
    (if(tab[i,4]=="2") (y4[i,1]="furniture/equipment"))  
    (if(tab[i,4]=="3") (y4[i,1]="radio/television"))  
    (if(tab[i,4]=="4") (y4[i,1]="domestic appliances"))  
    (if(tab[i,4]=="5") (y4[i,1]="repairs"))  
    (if(tab[i,4]=="6") (y4[i,1]="education"))  
    (if(tab[i,4]=="7") (y4[i,1]="vacation "))  
    (if(tab[i,4]=="8") (y4[i,1]="retraining"))  
    (if(tab[i,4]=="9") (y4[i,1]="business"))  
    (if(tab[i,4]=="10") (y4[i,1]="others"))  
    
  } 
  y6=data.frame(Savings_account=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,6]=="1") (y6[i,1]="...<100 DM"))  
    (if(tab[i,6]=="2") (y6[i,1]="100<=...<500 DM")) 
    (if(tab[i,6]=="3") (y6[i,1]="500<=...<1000 DM"))  
    (if(tab[i,6]=="4") (y6[i,1]="..>=1000 DM"))  
    (if(tab[i,6]=="5") (y6[i,1]="unknown/no savings account"))  
    
  }
  y7=data.frame(Present_employment_since=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,7]=="1") (y7[i,1]="unemployed"))  
    (if(tab[i,7]=="2") (y7[i,1]="...<1 year")) 
    (if(tab[i,7]=="3") (y7[i,1]="1<=...<4 years"))  
    (if(tab[i,7]=="4") (y7[i,1]="4<=...<7 years"))  
    (if(tab[i,7]=="5") (y7[i,1]="..>=7 years"))  
    
  }
  y9=data.frame(Personal_status_and_sex=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,9]=="1") (y9[i,1]="male:divorced/separated"))  
    (if(tab[i,9]=="2") (y9[i,1]="female:divorced/separated/married")) 
    (if(tab[i,9]=="3") (y9[i,1]="male:single"))  
    (if(tab[i,9]=="4") (y9[i,1]="male :married/widowed"))  
    (if(tab[i,9]=="5") (y9[i,1]="female:single"))  
    
  }
  y10=data.frame(Other_debtors_guarantors=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,10]=="1") (y10[i,1]="none"))  
    (if(tab[i,10]=="2") (y10[i,1]="co-applicant")) 
    (if(tab[i,10]=="3") (y10[i,1]="guarantor"))  
    
  }
  y12=data.frame(Property=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,12]=="1") (y12[i,1]="real estate"))  
    (if(tab[i,12]=="2") (y12[i,1]="if not 1:building society savings agreement/life insurance")) 
    (if(tab[i,12]=="3") (y12[i,1]="if not 1/2:car or other,not in attribute 6"))  
    (if(tab[i,12]=="4") (y12[i,1]="unknown / no property"))  
    
  }
  y14=data.frame(Other_installment_plans =1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,14]=="1") (y14[i,1]="bank"))  
    (if(tab[i,14]=="2") (y14[i,1]="stores")) 
    (if(tab[i,14]=="3") (y14[i,1]="non"))  
    
  }
  
  y15=data.frame(Housing=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,15]=="1") (y15[i,1]="rent"))  
    (if(tab[i,15]=="2") (y15[i,1]="own")) 
    (if(tab[i,15]=="3") (y15[i,1]="for free"))  
    
  }
  y17=data.frame(Job=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,17]=="1") (y17[i,1]="unemployed/ unskilled  - non-resident"))  
    (if(tab[i,17]=="2") (y17[i,1]="unskilled - resident")) 
    (if(tab[i,17]=="3") (y17[i,1]="skilled employee / official"))  
    (if(tab[i,17]=="4") (y17[i,1]="management/ self-employed/highly qualified employee/ officer"))  
    
  }
  y19=data.frame(Telephone=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,19]=="1") (y19[i,1]="none"))  
    (if(tab[i,19]=="2") (y19[i,1]="yes, registered under the customers name")) 
    
  }
  y20=data.frame(foreign_worker=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,20]=="1") (y20[i,1]="yes"))  
    (if(tab[i,20]=="2") (y20[i,1]="no"))
    
  }
  y21=data.frame(Good=1000)
  
  for (i in 1:1000) { 
    
    (if(tab[i,21]=="0") (y21[i,1]="yes"))  
    (if(tab[i,21]=="1") (y21[i,1]="no"))
    
  }
  output$distPlot <- renderPlot({
    
    choix<-input$var
    stat<-input$status
    dur<-input$duration
    creh<-input$credith
    pur<-input$purpose
    crea<-input$credita
    sav<-input$savings
    pree<-input$present
    ins<-input$installment
    pers<-input$personal
    oth<-input$other
    prer<-input$presentr
    pro<-input$property
    age<-input$age
    othi<-input$otheri
    hou<-input$housing
    num<-input$number
    job<-input$job
    numo<-input$numbero
    tel<-input$telephone
    fore<-input$foreign
    go<-input$good
    
    if(choix=="Status of existing checking account"){
      
      if(stat=="diagramme"){
        plot(table(y1),col=mycol, main="Status of existing checking account")
      }
      else if(stat=="camembert"){
        t1=ggplot(y1,aes(x="",fill=factor(Status_of_existing_checking_account)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Status of existing checking account") 
        
        
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y1$Status_of_existing_checking_account=="< 0 DM"),sum(y1$Status_of_existing_checking_account=="0 <= ... <200 DM"),sum(y1$Status_of_existing_checking_account=="... >= 200 DM "),sum(y1$Status_of_existing_checking_account=="no checking account"))) 
        
        pie<-t1+t3+c2+k1+ggtitle("la repartition de la population selon le statut du compte courant existant ")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        sond=cbind(tab,y1)
        pie
      } 
    }
    else if(choix=="Duration"){ 
      if(dur=="diagramme"){
        hist(tab$Duration ,col = grey(0.8),probability = T)
        ptx = seq(min(tab$Duration),max(tab$Duration), length=100)
        lines(ptx, dnorm(ptx, mean=mean(tab$Duration), sd=sd(tab$Duration)),lty=1,lwd=2,col="red")
      }
      else if(dur=="boxplot"){boxplot(tab$Duration, horizontal = TRUE, col="orange") }
    }
    else if(choix=="Credit history"){
      if(creh=="diagramme"){plot(table(y3),col=mycol, main="Credit history")}
      else if(creh=="camembert"){
        t1=ggplot(y3,aes(x="",fill=factor(y3$Credit_history)))+coord_polar(theta="y")+ geom_bar(width = 1)  
        t3=labs(x="",y="",fill="Credit history") 
        
        k1=annotate("text",label=c(sum(y3$Credit_history=="no credits taken/all credits paid back duly"),sum(y3$Credit_history=="all credits at this bank paid back duly"),sum(y3$Credit_history=="existing credits paid back duly till now"),sum(y3$Credit_history=="delay in paying off in the past"),sum(y3$Credit_history=="critical account/other credits existing (not at this bank)"))) 
        
        
        c2=scale_fill_brewer("",palette="Set2")    
        
        
        pie<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Credit history ")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        sond=cbind(tab,y3)
        pie
      }
    }
    else if(choix=="Purpose"){
      if(pur=="diagramme"){plot(table(y4),col=mycol, main="Purpose")}
      else if(pur=="camembert"){
        t1=ggplot(y4,aes(x="",fill=factor(y4$Purpose)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Status of existing checking account") 
        c2=scale_fill_brewer("",palette="Set3")    
        
        
        
        k1=annotate("text",label=c(sum(y4$Purpose=="car (new)"),sum(y4$Purpose=="car (used)"),sum(y4$Purpose=="furniture/equipment"),sum(y4$Purpose=="radio/television"),sum(y4$Purpose=="domestic appliances"))) 
        
        
        
        pi<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Purpose ")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        pi
      }}
    else if(choix=="Credit amount"){
      if(crea=="diagramme"){
        hist(tab$`Credit amount`,col = grey(0.8),probability =FALSE)
      }
      else if(crea=="boxplot"){boxplot(tab$`Credit amount`, horizontal = TRUE, col="orange")}
      
    }
    else if(choix=="Savings account/bonds"){
      if(sav=="diagramme"){plot(table(y6),col=mycol, main="Savings account/bonds")}
      else if(sav=="camembert"){
        t1=ggplot(y6,aes(x="",fill=factor(y6$Savings_account)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Status of existing checking account") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y6$Savings_account=="...<100 DM"),sum(y6$Savings_account=="100<=...<500 DM"),sum(y6$Savings_account=="500<=...<1000 DM"),sum(y6$Savings_account=="..>=1000 DM"),sum(y6$Savings_account=="unknown/no savings account"))) 
        
        p<- t1+t3+c2+k1+ggtitle("la repartition de la population selon Savings account/bonds ")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      } }
    else if(choix=="Present employment since"){
      if(pree=="diagramme"){plot(table(y7),col=mycol, main="Present employment since")}
      else if(pree=="camembert"){
        t1=ggplot(y7,aes(x="",fill=factor(y7$Present_employment_since)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y7$Present_employment_since=="unemployed"),sum(y7$Present_employment_since=="...<1 year"),sum(y7$Present_employment_since=="1<=...<4 years"),sum(y7$Present_employment_since=="4<=...<7 years"),sum(y7$Present_employment_since=="..>=7 years"))) 
        p<- t1+c2+k1+ggtitle("la repartition de la population selon Savings account/bonds ")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      }
    }
    else if(choix=="Installment rate in percentage of disposable income"){
      if(ins=="diagramme"){plot(table(tab$`Installment rate in percentage of disposable income`),col=mycol, main="")}
      
      else if(ins=="camembert"){
        t1=ggplot(tab,aes(x="",fill=factor(tab$`Installment rate in percentage of disposable income`)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Installment rate in percentage of disposable income") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(tab$`Installment rate in percentage of disposable income`=="1"),sum(tab$`Installment rate in percentage of disposable income`=="2"),sum(tab$`Installment rate in percentage of disposable income`=="3"),sum(tab$`Installment rate in percentage of disposable income`=="4"))) 
        
        p<- t1+t3+c2+k1+ggtitle("la repartition de la population selon Installment rate in percentage of disposable income ")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      }}
    else if(choix=="Personal status and sex"){
      if(pers=="diagramme"){plot(table(y9),col=mycol, main="Personal_status_and_sex")}
      else if(pers=="camembert"){
        t1=ggplot(y9,aes(x="",fill=factor(y9$Personal_status_and_sex)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Personal_status_and_sex") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y9$Personal_status_and_sex=="male:divorced/separated"),sum(y9$Personal_status_and_sex=="female:divorced/separated/married"),sum(y9$Personal_status_and_sex=="male:single"),sum(y9$Personal_status_and_sex=="male :married/widowed"),sum(y9$Personal_status_and_sex=="female:single"))) 
        
        p<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Personal_status_and_sex")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      }
    }
    else if(choix=="Other debtors / guarantors"){
      if(oth=="diagramme"){plot(table(y10),col=mycol, main="Other debtors / guarantors")}
      
      else if(oth=="camembert"){
        t1=ggplot(y10,aes(x="",fill=factor(y10$Other_debtors_guarantors)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Other debtors / guarantors") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y10$Other_debtors_guarantors=="none"),sum(y10$Other_debtors_guarantors=="co-applicant"),sum(y10$Other_debtors_guarantors=="guarantor")))
        
        
        p<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Other debtors / guarantors")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      }
    }
    else if(choix=="Present residence since"){
      if(prer=="diagramme"){plot(table(tab$`Present residence since`),col=mycol, main="Present residence since")}
      
      if(prer=="camembert"){
        t1=ggplot(tab,aes(x="",fill=factor(tab$`Present residence since`)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Present residence since") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(tab$`Present residence since`=="1"),sum(tab$`Present residence since`=="2"),sum(tab$`Present residence since`=="3"),sum(tab$`Present residence since`=="4")))
        
        
        p<- t1+t3+c2+k1+ggtitle("la repartition de la population selon Present residence since")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      }}
    else if(choix=="Property"){
      if(pro=="diagramme"){plot(table(y12),col=mycol, main="Property")}
      else if(pro=="camembert"){
        t1=ggplot(y12,aes(x="",fill=factor(y12$Property)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Property") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y12$Property=="real estate"),sum(y12$Property=="if not 1:building society savings agreement/life insurance"),sum(y12$Property=="if not 1/2:car or other,not in attribute 6"),sum(y12$Property=="unknown / no property")))
        
        
        p<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Property")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      }
    }
    else if(choix=="Age in years"){
      if(age=="diagramme"){plot(table(tab$`Age in years`),col=rainbow(10), main="Age in years")}
      else if(age=="boxplot"){boxplot(tab$`Age in years`, horizontal = TRUE, col="orange")}
    }
    
    else if(choix=="Other installment plans"){
      if(othi=="diagramme"){plot(table(y14),col=mycol, main="Other installment plans")}
      else if(othi=="camembert"){
        t1=ggplot(y14,aes(x="",fill=factor(y14$Other_installment_plans)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Other installment plans") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y14$Other_installment_plans=="bank"),sum(y14$Other_installment_plans=="stores"),sum(y14$Other_installment_plans=="non")))
        
        
        p<- t1+t3+c2+k1+ggtitle("la repartition de la population selon Other installment plans")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p  
      }
    }
    else if(choix=="Housing"){
      if(hou=="diagramme"){plot(table(y15),col=mycol, main="Housing ")
      }
      else if(hou=="camembert"){
        t1=ggplot(y15,aes(x="",fill=factor(y15$Housing)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Housing ") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y15$Housing=="rent"),sum(y15$Housing=="own"),sum(y15$Housing=="for free")))
        
        
        p<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Housing")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p  
      }
    }
    else if(choix=="Number of existing credits at this bank"){
      if(num=="diagramme"){hist(tab$`Number of existing credits at this bank`,col = grey(0.8),probability =TRUE)
        
        ptx = seq(min(tab$`Number of existing credits at this bank`),max(tab$`Number of existing credits at this bank`), length=100)
        lines(ptx, dnorm(ptx, mean=mean(tab$`Number of existing credits at this bank`), sd=sd(tab$`Number of existing credits at this bank`)),lty=1,lwd=2,col="red")
      }
      else if(num=="boxplot"){boxplot(tab$`Credit amount`, horizontal = TRUE, col="orange")}
    }
    else if(choix=="Job"){
      if(job=="diagramme"){plot(table(y17),col=mycol, main="Job")}
      
      else if(job=="camembert"){t1=ggplot(y17,aes(x="",fill=factor(y17$Job)))+coord_polar(theta="y")+ geom_bar(width = 1) 
      t3=labs(x="",y="",fill="Job") 
      
      c2=scale_fill_brewer("",palette="Set2")    
      k1=annotate("text",label=c(sum(y17$Job=="unemployed/ unskilled  - non-resident"),sum(y17$Job=="unskilled - resident"),sum(y17$Job=="skilled employee / official"),sum(y17$Job=="management/ self-employed/highly qualified employee/ officer")))
      
      
      p<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Job")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
      p
      }}
    else if(choix=="Number of people being liable to provide maintenance for"){
      if(numo=="diagramme"){hist(tab$`Number of people being liable to provide maintenance for`,col = grey(0.8),probability =TRUE)}
      else if(numo=="boxplot"){boxplot(tab$`Number of people being liable to provide maintenance for`, horizontal = TRUE, col="orange")}
    }
    else if(choix=="Telephone"){
      if(tel=="diagramme"){mycol<-c(  "red", "black","blue","green","magenta")}
      else if(tel=="camembert"){
        t1=ggplot(y19,aes(x="",fill=factor(y19$Telephone)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Telephone") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        
        k1=annotate("text",label=c(sum(y19$Telephone=="none"),sum(y19$Telephone=="yes, registered under the customers name")))
        
        
        p<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Telephone")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      } }
    else if(choix=="foreign worker"){
      if(fore=="diagramme"){plot(table(y20),col=mycol, main="foreign worker")}
      else if(fore=="camembert"){
        t1=ggplot(y20,aes(x="",fill=factor(y20$foreign_worker)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="foreign worker") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y20$foreign_worker=="yes"),sum(y20$foreign_worker=="no")))
        
        
        p<- t1+t3+c2+k1+ggtitle("la repartition de la population selon foreign worker")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      } }
    else if(choix=="Good"){
      if(go=="diagramme"){plot(table(y21),col=mycol, main="Good")}
      else if(go=="camembert"){
        t1=ggplot(y21,aes(x="",fill=factor(y21$Good)))+coord_polar(theta="y")+ geom_bar(width = 1) 
        t3=labs(x="",y="",fill="Good") 
        
        c2=scale_fill_brewer("",palette="Set2")    
        k1=annotate("text",label=c(sum(y21$Good=="yes"),sum(y21$Good=="no")))
        p<-t1+t3+c2+k1+ggtitle("la repartition de la population selon Good")+theme(plot.title = element_text(color="black", size=16, face="bold")) 
        p
      }
    }
  })
  
  
  
  output$summary<-renderPrint({
    choix<-input$var
    stat<-input$status
    dur<-input$duration
    creh<-input$credith
    pur<-input$purpose
    crea<-input$credita
    sav<-input$savings
    pree<-input$present
    ins<-input$installment
    pers<-input$personal
    oth<-input$other
    prer<-input$presentr
    pro<-input$property
    age<-input$age
    othi<-input$otheri
    hou<-input$housing
    num<-input$number
    job<-input$job
    numo<-input$numbero
    tel<-input$telephone
    fore<-input$foreign
    go<-input$good
    if(choix=="Status of existing checking account"){
      
      if(stat=="summary"){
        freq(y1$Status_of_existing_checking_account)
      }}
    else if(choix=="Duration"){
      if(dur=="summary"){summary(tab$Duration)}}
    else if(choix=="Credit history"){
      if(creh=="summary"){freq(y3, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}}
    else if(choix=="Purpose"){
      if(pur=="summary"){freq(y4, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}}
    else if(choix=="Credit amount"){
      if(crea=="summary"){summary(tab$`Credit amount`)}}
    else if(choix=="Savings account/bonds"){
      if(sav=="summary"){freq(y6, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}}
    else if(choix=="Present employment since"){
      if(pree=="summary"){freq(y7, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}}
    else if(choix=="Installment rate in percentage of disposable income"){
      if(ins=="summary"){freq(tab$`Installment rate in percentage of disposable income`, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}}  
    else if(choix=="Personal status and sex"){
      if(pers=="summary"){freq(y9, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    else if(choix=="Other debtors / guarantors"){
      if(oth=="summary"){freq(y10, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    else if(choix=="Present residence since"){
      if(prer=="summary"){freq(tab$`Present residence since`, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    else if(choix=="Property"){
      if(pro=="summary"){freq(y12, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    else if(choix=="Age in years"){
      if(age=="summary"){summary(tab$`Age in years`)}} 
    else if(choix=="Other installment plans"){
      if(othi=="summary"){freq(y14, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    
    else if(choix=="Housing"){
      if(hou=="summary"){freq(y15, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    
    else if(choix=="Number of existing credits at this bank"){
      if(num=="summary"){summary(tab$`Number of existing credits at this bank`)}} 
    
    else if(choix=="Job"){
      if(job=="summary"){freq(y17, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    
    else if(choix=="Number of people being liable to provide maintenance for"){
      if(numo=="summary"){summary(tab$`Number of people being liable to provide maintenance for`)}} 
    
    else if(choix=="Telephone"){
      if(tel=="summary"){freq(y19, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    else if(choix=="foreign worker"){
      if(fore=="summary"){freq(y20, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    else if(choix=="Good"){
      if(go=="summary"){freq(y21, cum = TRUE, total = TRUE, sort = "inc",digits = 2, exclude = NA)}} 
    
  })
  
  output$di <- renderPlot({
    
    ch<-input$choix
    sta<-input$stat
    du<-input$Dur
    cre<-input$cred
    pu<-input$purp
    cred<-input$creda
    sa<-input$sava
    pre<-input$prese
    inst<-input$installr
    per<-input$persos
    ot<-input$othrd
    pres<-input$presr
    pr<-input$proper
    ag<-input$agei
    oth<-input$othri
    ho<-input$hous
    num<-input$numro
    joo<-input$jo
    nu<-input$numop
    te<-input$tele
    fo<-input$forei
    
    if(ch=="Status of existing checking account"){
      if(sta=="plot"){
        qplot(factor(y1$Status_of_existing_checking_account),geom="bar",fill=factor(y21$Good))
      }}
    else if(ch=="Duration"){
      if(du=="plot"){
        qplot(factor(tab$Duration),geom="bar",fill=factor(y21$Good))
      }}
    else if(ch=="Credit history"){
      if(cre=="plot"){qplot(factor(y3$Credit_history),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Purpose"){
      if(pu=="plot"){qplot(factor(y4$Purpose),geom="bar",fill=factor(y21$Good))}}
    
    else if(ch=="Savings account/bonds"){
      if(sa=="plot"){qplot(factor(y6$Savings_account),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Present employment since"){
      if(pre=="plot"){qplot(factor(y7$Present_employment_since),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Installment rate in percentage of disposable income"){
      if(inst=="plot"){qplot(factor(tab$`Installment rate in percentage of disposable income`),geom="bar",fill=factor(y21$Good))
      }} 
    else if(ch=="Personal status and sex"){
      if(per=="plot"){qplot(factor(y9$Personal_status_and_sex),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Other debtors / guarantors"){
      if(ot=="plot"){qplot(factor(y10$Other_debtors_guarantors),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Present residence since"){
      if(pres=="plot"){qplot(factor(tab$`Present residence since`),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Property"){
      if(pr=="plot"){qplot(factor(y12$Property),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Age in years"){
      if(ag=="plot"){qplot(factor(tab$`Age in years`),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Other installment plans"){
      if(oth=="plot"){qplot(factor(y14$Other_installment_plans),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Housing"){
      if(ho=="plot"){qplot(factor(y15$Housing),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Number of existing credits at this bank"){
      if(num=="plot"){qplot(factor(tab$`Number of existing credits at this bank`),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Job"){
      if(joo=="plot"){qplot(factor(y15$Housing),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="Number of people being liable to provide maintenance for"){
      if(nu=="plot"){qplot(factor(tab$`Number of people being liable to provide maintenance for`),geom="bar",fill=factor(y21$Good))
      }}
    else if(ch=="Telephone"){
      if(te=="plot"){qplot(factor(y19$Telephone),geom="bar",fill=factor(y21$Good))}}
    else if(ch=="foreign worker"){
      if(fo=="plot"){qplot(factor(y20$foreign_worker),geom="bar",fill=factor(y21$Good))}}
    
  })
  
  
  output$ta<-renderPrint({
    ch<-input$choix
    sta<-input$stat
    du<-input$Dur
    cre<-input$cred
    pu<-input$purp
    cred<-input$creda
    sa<-input$sava
    pre<-input$prese
    inst<-input$installr
    per<-input$persos
    ot<-input$othrd
    pres<-input$presr
    pr<-input$proper
    ag<-input$agei
    oth<-input$othri
    ho<-input$hous
    num<-input$number
    joo<-input$jo
    nu<-input$numop
    te<-input$tele
    fo<-input$forei
    if(ch=="Status of existing checking account"){
      if(sta=="table"){
        p1=table(y1$Status_of_existing_checking_account,y21$Good)
        
        prop.table(p1)
        
      }}
    else if(ch=="Duration"){
      if(du=="table"){
        p2=table(tab$Duration,y21$Good)
        prop.table(p2)}}
    else if(ch=="Credit history"){
      if(cre=="table"){p3=table(y3$Credit_history,y21$Good)
      prop.table(p3)}}
    
    else if(ch=="Purpose"){
      if(pu=="table"){p4=table(y4$Purpose,y21$Good)
      prop.table(p4)}}
    else if(ch=="Credit amount"){
      if(cred=="table"){
        p5=table(tab$`Credit amount`,y21$Good)
        prop.table(p5)}}
    
    
    else if(ch=="Savings account/bonds"){
      if(sa=="table"){
        p6=table(y6$Savings_account,y21$Good)
        prop.table(p6)}}
    else if(ch=="Present employment since"){
      if(pre=="table"){
        p7=table(y7$Present_employment_since,y21$Good)
        prop.table(p7)
      }}
    else if(ch=="Installment rate in percentage of disposable income"){
      if(inst=="table"){
        p8=table(tab$`Installment rate in percentage of disposable income`,y21$Good)
        prop.table(p8)
        
      }} 
    else if(ch=="Personal status and sex"){
      if(per=="table"){
        p9=table(y9$Personal_status_and_sex,y21$Good)
        prop.table(p9)}}
    else if(ch=="Other debtors / guarantors"){
      if(ot=="table"){
        p10=table(y10$Other_debtors_guarantors,y21$Good)
        prop.table(p10)}}
    else if(ch=="Present residence since"){
      if(pres=="table"){
        p11=table(tab$`Present residence since`,y21$Good)
        prop.table(p11)}}
    else if(ch=="Property"){
      if(pr=="table"){
        p12=table(y12$Property,y21$Good)
        prop.table(p12)}}
    else if(ch=="Age in years"){
      if(ag=="table"){
        p13=table(tab$`Age in years`,y21$Good)
        prop.table(p13)}}
    else if(ch=="Other installment plans"){
      if(oth=="table"){
        p14=table(y14$Other_installment_plans,y21$Good)
        prop.table(p14)}}
    else if(ch=="Housing"){
      if(ho=="table"){
        p15=table(y15$Housing,y21$Good)
        prop.table(p15)}}
    else if(ch=="Number of existing credits at this bank"){
      if(num=="table"){
        qp16=table(tab$`Number of existing credits at this bank`,y21$Good)
        prop.table(p16)}}
    else if(ch=="Job"){
      if(joo=="table"){
        p17=table(y17$Job,y21$Good)
        prop.table(p17)}}
    else if(ch=="Number of people being liable to provide maintenance for"){
      if(nu=="table"){
        p18=table(tab$`Number of people being liable to provide maintenance for`,tab$Good)
        prop.table(p18)
      }}
    else if(ch=="Telephone"){
      if(te=="table"){
        p19=table(y19$Telephone,y21$Good)
        prop.table(p19)
      }}
    else if(ch=="foreign worker"){
      if(fo=="ptable"){
        p20=table(y19$Telephone,y21$Good)
        prop.table(p20)}}
    
  })
  library(Deducer)
  library(caret)
  library(ROCR)
  library(pROC)
  library(xlsx) 
  
  tab$`Status of existing checking account`=factor(tab$`Status of existing checking account`)
  tab$`Credit history`=factor(tab$`Credit history`)
  tab$Purpose=factor(tab$Purpose)
  tab$`Savings account/bonds`=factor(tab$`Savings account/bonds`)
  tab$`Present employment since`=factor(tab$`Present employment since`)
  tab$`Personal status and sex`=factor(tab$`Personal status and sex`)
  tab$`Other debtors / guarantors`=factor(tab$`Other debtors / guarantors`)
  tab$Property=factor(tab$Property)
  tab$`Other installment plans`=factor(tab$`Other installment plans`)
  tab$Housing=factor(tab$Housing)
  tab$Job=factor(tab$Job)
  tab$Telephone=factor(tab$Telephone)
  tab$`foreign worker`=factor(tab$`foreign worker`)
  tab$Good=factor(tab$Good)
  
  train_id=sample(1000,750)
  data_train=tab[train_id,]
  data_test=tab[-train_id,]
  
  cvControl=trainControl(method="cv",number=10)
  logit=glm(Good~.,data=data_train,family=binomial)
  
  glmFit<-train(Good~.,data=data_train,
                method = "glmStepAIC",trControl = cvControl)
  
  nnetFit<-train(Good~.,data=data_train,
                 method = "nnet", tuneLength =6,
                 trControl = cvControl)
  
  library(rpart)
  rpartFit=rpart(Good~.,data_train,control = rpart.control(cp = 0.00001))
  rpartFit
  
  #4 Random forest
  
  rfFit = train(Good~.,data=data_train,
                method = "rf", tuneLength = 4,
                trControl = cvControl)
  
  
  #5 boosting
  set.seed(2)
  gbmFit = train(Good~.,data=data_train,
                 method = "gbm", tuneLength = 4,
                 trControl = cvControl)
  
  #6 boosting 2
  set.seed(2)
  c50Fit = train(Good~.,data=data_train,
                 method = "C5.0", tuneLength = 4,
                 trControl = cvControl)
  
  
  
  
  
  
  
  
  ###########################
  output$trr<-renderPrint({
    
    cho<-input$cho
    re<-input$res
    ar<-input$arb
    ra<-input$ran
    b1<-input$boo1
    b2<-input$boo2
    
    if(cho=="Selection backward"){
      
      glmFit$finalModel
    }
    else if(cho=="Reseaux de neurones"){
      if(re=="resume"){nnetFit}
    }
    else if(cho=="Arbre de decision"){
      if(ar=="resume"){rpartFit}
    }
    else if(cho=="Random forest"){
      if(ra=="resume"){rfFit}
    }
    else if(cho=="Boosting1"){
      if(b1=="resume"){gbmFit}
    }
    else if(cho=="Boosting2"){
      if(b2=="resume"){c50Fit}
    }
    
  })
  output$digg<-renderPlot({
    
    cho<-input$cho
    re<-input$res
    ar<-input$arb
    ra<-input$ran
    b1<-input$boo1
    b2<-input$boo2
    
    if(cho=="Reseaux de neurones"){
      if(re=="plot"){plot(nnetFit)}
    }
    else if(cho=="Arbre de decision"){
      if(ar=="plot"){
        plot(rpartFit)+text(rpartFit)
        plotcp(rpartFit)
      }
    }
    else if(cho=="Random forest"){
      if(ra=="plot"){
        plot(rfFit)
      }
    }
    else if(cho=="Boosting1"){
      if(b1=="plot"){
        plot(gbmFit)
      }
    }
    else if(cho=="Boosting2"){
      if(b2=="plot"){
        plot(c50Fit)
      }
    }
  })
  models=list(rlog=glmFit,nnet=nnetFit,rtree=rpartFit,
              rf=rfFit,gbm=gbmFit,C50=c50Fit)
  
  library(ROCR)
  testProb=predict(models, newdata = data_test,
                   type="prob")
  
  trainProb=predict(models, newdata = data_train,
                    type="prob")
  testpredroc=lapply(testProb,
                     function(x)prediction(x[,1],
                                           data_test[,"Good"]=="1"))
  trainpredroc=lapply(trainProb,
                      function(x)prediction(x[,1],
                                            data_train[,"Good"]=="1"))
  
  testperfroc=lapply(testpredroc,
                     function(x)performance(x, "tpr", "fpr"))
  trainperfroc=lapply(trainpredroc,
                      function(x)performance(x, "tpr", "fpr"))
  
  
  auc_test=lapply(testpredroc,
                  function(x)performance(x, measure = "auc"))
  auc_train=lapply(trainpredroc,
                   function(x)performance(x, measure = "auc"))
  
  # Tracer les courbes ROC
  
  
  output$dd<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(testperfroc$rlog,col=1)
    plot(testperfroc$nnet,col=2,add=TRUE)
    plot(testperfroc$rtree,col=3,add=TRUE)
    
    plot(testperfroc$rf,col=4,add=TRUE)
    plot(testperfroc$gbm,col=5,add=TRUE)
    plot(testperfroc$C50,col=6,add=TRUE)
    legend("bottomright",legend=c("rlog","nnet",
                                  "Tree","RF","gbm","C50"),col=c(1:6),pch="_")
    
    
  })
  output$gg<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$rlog,col=1)
    plot(trainperfroc$nnet,col=2,add=TRUE)
    plot(trainperfroc$rtree,col=3,add=TRUE)
    plot(trainperfroc$rf,col=4,add=TRUE)
    plot(trainperfroc$gbm,col=5,add=TRUE)
    plot(trainperfroc$C50,col=6,add=TRUE)
    legend("bottomright",legend=c("rlog","nnet",
                                  "Tree","RF","gbm","C50"),col=c(1:6),pch="_")
    
  })
  
  output$glmfit<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$rlog,col=1)
    plot(testperfroc$rlog,col=2,add=TRUE)
    
    legend("bottomright",legend=c("train","test"),col=c(1:6),pch="_")
    text(x=0.4,y=0.5,paste("auc=",round(as.numeric(auc_train$rlog@y.values),digits = 3)),col =1)
    text(x=0.4,y=0.4,paste("auc=",round(as.numeric(auc_test$rlog@y.values),digits = 3)),col =2)
    
    
  })
  output$nnetfit<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$nnet,col=1)
    plot(testperfroc$nnet,col=2,add=TRUE)
    
    legend("bottomright",legend=c("train","test"),col=c(1:6),pch="_")
    text(x=0.4,y=0.5,paste("auc=",round(as.numeric(auc_train$rlog@y.values),digits = 3)),col =1)
    text(x=0.4,y=0.4,paste("auc=",round(as.numeric(auc_test$rlog@y.values),digits = 3)),col =2)
    
    
  })
  output$rpart<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$rtree,col=1)
    plot(testperfroc$rtree,col=2,add=TRUE)
    
    legend("bottomright",legend=c("train","test"),col=c(1:6),pch="_")
    text(x=0.4,y=0.5,paste("auc=",round(as.numeric(auc_train$rlog@y.values),digits = 3)),col =1)
    text(x=0.4,y=0.4,paste("auc=",round(as.numeric(auc_test$rlog@y.values),digits = 3)),col =2)
    
    
  })
  output$rpart<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$rtree,col=1)
    plot(testperfroc$rtree,col=2,add=TRUE)
    
    legend("bottomright",legend=c("train","test"),col=c(1:6),pch="_")
    text(x=0.4,y=0.5,paste("auc=",round(as.numeric(auc_train$rlog@y.values),digits = 3)),col =1)
    text(x=0.4,y=0.4,paste("auc=",round(as.numeric(auc_test$rlog@y.values),digits = 3)),col =2)
    
    
  })
  output$rf<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$rf,col=1)
    plot(testperfroc$rf,col=2,add=TRUE)
    
    legend("bottomright",legend=c("train","test"),col=c(1:6),pch="_")
    text(x=0.4,y=0.5,paste("auc=",round(as.numeric(auc_train$rlog@y.values),digits = 3)),col =1)
    text(x=0.4,y=0.4,paste("auc=",round(as.numeric(auc_test$rlog@y.values),digits = 3)),col =2)
  })
  output$gbm<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$gbm,col=1)
    plot(testperfroc$gbm,col=2,add=TRUE)
    
    legend("bottomright",legend=c("train","test"),col=c(1:6),pch="_")
    text(x=0.4,y=0.5,paste("auc=",round(as.numeric(auc_train$rlog@y.values),digits = 3)),col =1)
    text(x=0.4,y=0.4,paste("auc=",round(as.numeric(auc_test$rlog@y.values),digits = 3)),col =2)
  })
  output$c50fit<-renderPlot({
    par(mfrow = c(1, 1))
    
    plot(trainperfroc$C50,col=1)
    plot(testperfroc$C50,col=2,add=TRUE)
    
    legend("bottomright",legend=c("train","test"),col=c(1:6),pch="_")
    text(x=0.4,y=0.5,paste("auc=",round(as.numeric(auc_train$rlog@y.values),digits = 3)),col =1)
    text(x=0.4,y=0.4,paste("auc=",round(as.numeric(auc_test$rlog@y.values),digits = 3)),col =2)
  })
  
  
})