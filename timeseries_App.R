
# set up -----------------------------------------------------------------------
# load packages that will be used for the application
library(shiny)
library(shinythemes)
library(rstan)
library(rstanarm)
library(readr)
library(gridExtra)
library(gtable)
library(grid)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)
library(markdown)
library(knitr)

ErrorMetrics <- read_csv("./Error Metrics - Sheet1.csv")
Full_AR_Results <- read_csv("./Full_AR_Results.csv")
Full_GO_Results <- read_csv("./Full_GO_Results.csv")
Full_AR5_Results <- read_csv("./Full_AR5_Results.csv")
load(file="GOMall.rda")
load(file="ARMall.rda")
load(file="./AR5all.rda")
Parks<- read_csv("./Yearly Visitation Data - Sheet1.csv")



# Set up the application ui
ui<-shinyUI(navbarPage("NPS Visitation Forecast Explorer",
                       
                       # define the tabs to be used in the app ----------------------------------------
                       # introduction splash
                       tabPanel("Intro",
                                h1("NPS Visitation Forecast Explorer"),
                                br(),
                                h3("This application is designed to be coupled with DESCRIPTION/LINK TO OUR PAPER"),
                                p("This application allows users to compare two different types of predictive models for visitation forecasting in US
                                  National Parks. One model is informed by Google Trends values alone and the other is informed by previous visitation alone
                                  (auoregressive model). Users can then predict future visitation for all parks using the model of their choice"),
                                br(),
                                br(),
                                h2("Authorship Information"),
                                p("This app was created by Matt Clark, WHATEVER OTHER INFO HERE"),
                                hr()),
                       
                       # Model Validation
                       tabPanel("Model validation",
                                # plot the map
                                fluidRow(column(12,
                                                h1("Model Validation"),
                                                p("Tool for visually analyzing the accuracy of both model types for each park. Credibility intervals (C.I.) represent the inner 25th and 50th percentiles of 2000 visitation estimates for each park."),
                                                br(),
                                                strong("Visitation estimates are informed by data prior to estimated year only."))),
                                         
                                         hr(),
                                         fluidRow(sidebarPanel(width = 3,
                                                               
                                                               helpText("Which models and park do you want to look at?."),
                                                               selectInput(inputId = "Type", label = "Choose Park",choices=c("ACAD", "ARCH" ,"BADL", "BIBE" ,"BISC" ,"BLCA", "BRCA", "CANY" ,"CARE", "CAVE", "CHIS", "CONG", "CRLA" ,"CUVA", "DEVA", "DENA", "DRTO" ,"EVER" ,"GAAR" ,"GLBA", "GLAC", "GRCA",
                                                                                                                             "GRTE", "GRBA", "GRSA", "GRSM", "GUMO" ,"HALE" ,"HAVO", "HOSP", "ISRO" ,"JOTR", "KATM" ,"KEFJ", "KICA", "KOVA", "LACL", "LAVO" ,"MACA" ,"MEVE", "MORA" ,"NPSA", "NOCA", "OLYM",
                                                                                                                             "PEFO", "REDW", "ROMO" ,"SAGU", "SEQU", "SHEN", "THRO", "VIIS", "VOYA", "WICA", "WRST", "YELL" ,"YOSE" ,"ZION")),
                                                               radioButtons(inputId = "ModChoice", label = "Model Comparison",choices = c("Google vs 2 Year Autoregressive","Google vs 5 Year Autoregressive","2 vs 5 Year Autoregressive","All Models"))),
                                                               
                                                              mainPanel(plotOutput("myplot",width="800px",height = "600px")))
                                ),
                                                  
                                         
                       #width="1200px",height = "550px"
                       
                       # forecasting tool
                       tabPanel("Forecasting tool",
                                fluidRow(column(12,
                                                h1("Forecasting Tool"),
                                                p("The Google Trends model and autoregressive model predict with different accuracies for different parks, under different circumstances. Determine which is best for your needs using the 'Model validation' tab and start forecasting!"),
                                                br(),
                                                h4("Instructions"),
                                                p("Use the control panel on the left to chose a park and inform your predictions. Note that the Google Trends slider only affects the Google Trends model and the previous visitation input only affects the autoregressive model"))),
                                hr(),
                                fluidRow(sidebarPanel(width = 3,
                                                      h4("Model type"),
                                                      helpText("Chose which type of model you want to use and inform your predictions."),
                                                      selectInput(inputId = "ModelType",label = "Model Type",choices=c("Google Trends","2 Year Autoregressive", "5 Year Autoregressive")),
                                                      selectInput(inputId = "Park", label = "Choose Park",choices=c("ACAD", "ARCH" ,"BADL", "BIBE" ,"BISC" ,"BLCA", "BRCA", "CANY" ,"CARE", "CAVE", "CHIS", "CONG", "CRLA" ,"CUVA", "DEVA", "DENA", "DRTO" ,"EVER" ,"GAAR" ,"GLBA", "GLAC", "GRCA",
                                                                                                                    "GRTE", "GRBA", "GRSA", "GRSM", "GUMO" ,"HALE" ,"HAVO", "HOSP", "ISRO" ,"JOTR", "KATM" ,"KEFJ", "KICA", "KOVA", "LACL", "LAVO" ,"MACA" ,"MEVE", "MORA" ,"NPSA", "NOCA", "OLYM",
                                                                                                                    "PEFO", "REDW", "ROMO" ,"SAGU", "SEQU", "SHEN", "THRO", "VIIS", "VOYA", "WICA", "WRST", "YELL" ,"YOSE" ,"ZION")), 
                                                      
                                                      sliderInput(inputId ="Google", label = "Yearly Total Google Trends Value (sum of all months)", min = 100, step=50,max = 1000, value = 300),
                                                      numericInput(inputId = "Vis1", label = "Last year's total visitation", value = 1000000),
                                                      numericInput(inputId = "Vis2", label = "Two years ago total visitation", value = 1000000),
                                         numericInput(inputId = "Vis3", label = "Three years ago total visitation", value = 1000000),
                                         numericInput(inputId = "Vis4", label = "Four years ago total visitation", value = 1000000),
                                         numericInput(inputId = "Vis5", label = "Five years ago total visitation", value = 1000000)),
                                         mainPanel(plotOutput("Forecast", height = 500)))
                       ),
                       
                       
                       # simple data table output
                       tabPanel("Error Metrics by Park",
                                column(12,
                                       h1("Error Metrics by Park"),
                                       p("This tabs shows the R^2, root mean squared error (RMSE), and mean absolute error (MEA) for the median visitation estimate for each park and the observed visitation for each park. The overall error estimates for each model are also displayed"
                                       ),
                                       column(10, dataTableOutput("table", height = "100%")))),
                       
                       
                       #Raw data
                       tabPanel("Data",
                                column(12,
                                       h1("Data"),
                                       p("This is the complete dataframe used to inform these models. All park visitation data was collected from the National Park Visitor Use Statistics Portal."),
                                       
                                       column(10, dataTableOutput("data", height = "100%"))))
                       
                       
                       # close the UI definition
))



server <- function(input, output) {
  output$myplot <- renderPlot({
    
    minnimum<-(min(Full_GO_Results[Full_GO_Results$Park==input$Type,]$LowerBound50)/1.5)
    maximum<-  (max(Full_AR_Results[Full_AR_Results$Park==input$Type,]$UpperBound50)*1.5)
    
    p1<-(ggplot(Full_GO_Results[Full_GO_Results$Park==input$Type,], aes(x=Year))+
           geom_ribbon(aes(ymin=LowerBound50, ymax=UpperBound50,fill="#9e9ac8"),alpha=0.6)+
           geom_ribbon(aes(ymin=LowerBound25, ymax=UpperBound25,fill="#54278f"),alpha=0.6)+
           geom_line(aes(y=Visitation, x=Year,color="black"), linetype="dashed" ,size=3)+
           labs(title="Google Trends Model",y=NULL,
                x=NULL)+
           scale_y_continuous(labels = scales::comma,limits = c(minnimum, maximum))+
           scale_x_discrete(limits=c(2013,2015,2017))+
           theme(axis.text.x = element_blank(), 
                 axis.text.y = element_text(vjust=0.5, size=22, angle=0),
                 plot.title=element_text(size=20, face="bold"),
                 panel.border = element_blank(), axis.line = element_line(),
                 panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                 axis.title=element_text(size=20,face="bold"), legend.position = "bottom", 
                 legend.text = element_text(size=15),legend.key = element_rect(size = 5, color = 'white'),
                 legend.key.size = unit(1.2, 'lines'))+
           scale_fill_identity( guide = 'legend',name=NULL, labels = c('50% C.I.     ', "95% C.I.")) +
           scale_colour_manual(name = NULL, values =c('black'='black','#6CC4EE'='#6CC4EE',"red"="red"), 
                               labels = c("Observed Visitation  ","Observed Visitation","Autoregressive Model")) )
    
    
    p2<-(ggplot(Full_AR_Results[Full_AR_Results$Park==input$Type,], aes(x=Year))+
           geom_ribbon(aes(ymin=LowerBound50, ymax=UpperBound50,fill="#f6546a"),alpha=0.6)+
           geom_ribbon(aes(ymin=LowerBound25, ymax=UpperBound25,fill="#a91818"),alpha=0.6)+
           geom_line(aes(y=Visitation, x=Year,color="black"), linetype="dashed" ,size=3)+
           labs(title="2 Year Autoregressive Model",y=NULL,
                x=NULL)+
           scale_y_continuous(labels = scales::comma,limits=c(minnimum, maximum))+
           scale_x_discrete(limits=c(2013,2015,2017))+
           theme(axis.text.x = element_text( vjust=0.5, size=22, angle=0), 
                 axis.text.y = element_text(vjust=0.5, size=22, angle=0),
                 plot.title=element_text(size=20, face="bold"),
                 panel.border = element_blank(), axis.line = element_line(),
                 panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                 axis.title=element_text(size=20,face="bold"), legend.position = "bottom", 
                 legend.text = element_text(size=15),legend.key = element_rect(size = 5, color = 'white'),
                 legend.key.size = unit(1.2, 'lines'))+
           scale_fill_identity( guide = 'legend',name=NULL, labels = c('50% C.I.     ', "95% C.I.")) +
           scale_colour_manual(name = NULL, values =c('black'='black','#6CC4EE'='#6CC4EE',"red"="red"), 
                               labels = c("Observed Visitation  ","Observed Visitation","Autoregressive Model")))
    
    p3<-(ggplot(Full_AR5_Results[Full_AR5_Results$Park==input$Type,], aes(x=Year))+
           geom_ribbon(aes(ymin=LowerBound50, ymax=UpperBound50,fill="#f6546a"),alpha=0.6)+
           geom_ribbon(aes(ymin=LowerBound25, ymax=UpperBound25,fill="#a91818"),alpha=0.6)+
           geom_line(aes(y=Visitation, x=Year,color="black"), linetype="dashed" ,size=3)+
           labs(title="5 Year Autoregressive Model",y=NULL,
                x=NULL)+
           scale_y_continuous(labels = scales::comma,limits=c(minnimum, maximum))+
           scale_x_discrete(limits=c(2013,2015,2017))+
           theme(axis.text.x = element_text( vjust=0.5, size=22, angle=0), 
                 axis.text.y = element_text(vjust=0.5, size=22, angle=0),
                 plot.title=element_text(size=20, face="bold"),
                 panel.border = element_blank(), axis.line = element_line(),
                 panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                 axis.title=element_text(size=20,face="bold"), legend.position = "bottom", 
                 legend.text = element_text(size=15),legend.key = element_rect(size = 5, color = 'white'),
                 legend.key.size = unit(1.2, 'lines'))+
           scale_fill_identity( guide = 'legend',name=NULL, labels = c('50% C.I.     ', "95% C.I.")) +
           scale_colour_manual(name = NULL, values =c('black'='black','#6CC4EE'='#6CC4EE',"red"="red"), 
                               labels = c("Observed Visitation  ","Observed Visitation","Autoregressive Model")))
    
    
    
    label = textGrob("Visitors", rot = 90, vjust = 0.5 ,gp = gpar(fontface = "bold", cex = 2))
    label2 = textGrob("Year", rot = 0, vjust = 0.5 ,gp = gpar(fontface = "bold", cex = 2))
    legend = gtable_filter(ggplotGrob(p1), "guide-box")
    
    
    
    
    
    
    
    
    if (input$ModChoice == "Google vs 2 Year Autoregressive") {grid.arrange( arrangeGrob(p1,
                                                                                              p2 ,
                                                                                              nrow=2),left=label, bottom=label2)}
    else if (input$ModChoice == "Google vs 5 Year Autoregressive"){grid.arrange( arrangeGrob(p1 ,
                                                                                                  p3,
                                                                                                  nrow=2), left=label,bottom=label2)}
    else if (input$ModChoice == "2 vs 5 Year Autoregressive"){grid.arrange( arrangeGrob(p2 + theme(axis.text.x=element_blank()),
                                                                                             p3,
                                                                                             nrow=2), left=label,bottom=label2)}
    else if (input$ModChoice == "All Models"){grid.arrange( arrangeGrob(p1 ,
                                                                        p2+ theme(axis.text.x=element_blank()) ,
                                                                                             p3 ,
                                                                                             nrow=3 ),left=label, bottom=label2)}
    
  })
  
  
  #Make Forecast Plots
  output$Forecast<- renderPlot({
    color=rgb(0,0,0,alpha=0.3)
    
    arcol<-rgb(169/255, 24/255, 24/255,alpha=0.6)
    gocol<-rgb(84/255,39/255,143/255,alpha=0.6)
    
    
    n1 <- 25 
    n2<-75
    
    
    
    postGO<-(posterior_predict(
      GOMall,data.frame(Google=input$Google,Park=input$"Park"),draws=2000))
    
    hGO = hist(postGO,breaks=50,plot=FALSE) 
    hGO$density = hGO$counts/sum(hGO$counts)*100
    
    
    
    postAR<-(posterior_predict(
      ARMall,data.frame(VisLag1=input$Vis1,
                        VisLag2=input$Vis2,Park=input$"Park"),draws=2000))
    
    hAR = hist(postAR,breaks=50,plot=FALSE) 
    hAR$density = hAR$counts/sum(hAR$counts)*100
    
    postAR5<-(posterior_predict(
      AR5ALL,data.frame(VisLag1=input$Vis1,
                        VisLag2=input$Vis2,
                        VisLag3=input$Vis3,
                        VisLag4=input$Vis4,
                        VisLag5=input$Vis5,
                        Park=input$"Park"),draws=2000))
    
    hAR5 = hist(postAR5,breaks=50,plot=FALSE) 
    hAR5$density = hAR5$counts/sum(hAR5$counts)*100
    
    medGO<-round(median(postGO),2)
    medAR<-round(median(postAR),2)
    medAR5<-round(median(postAR5),2)
    
    minGO<-min(postGO)
    maxGO<- max(postGO)
    minAR<-min(postAR)
    maxAR<-max(postAR)
    minAR5<-min(postAR5)
    maxAR5<-max(postAR5)
    
    if (input$ModelType == "Google Trends") {
      plot(hGO, main=paste("Median Visitation Estimate = ", format(medGO,big.mark = ",")),
           xlab="Number of Total Visitors", xlim=c(minGO,maxGO),ylab="Percentage of Samples",col=gocol,freq=FALSE)
      abline(v = median(postGO),
             col = "black",
             lwd = 3)
      
      upbound1<-min(postGO[postGO > quantile(postGO,prob=1-n1/100),])
      
      abline(v=upbound1,col=color,lty=2,lwd=3)
      
      lwrbound1<-max(postGO[postGO<quantile(postGO,prob=1-n2/100),])
      abline(v=lwrbound1,col=color,lty=2,lwd=3)
      
      
      legend(x = "topright", 
             c("Median",paste("50% Credibility Interval:",format(lwrbound1,big.mark=","), "-" ,format(upbound1,big.mark=","))),
             col =  c("black",color),lty=c(1,2),
             lwd = c(2,2))}
    
    else if (input$ModelType == "2 Year Autoregressive") {
      plot(hAR, main=paste("Median Visitation Estimate = ", format(medAR,big.mark = ",")),
           xlab="Number of Total Visitors", xlim=c(minAR,maxAR),ylab="Percentage of Samples",col=arcol,freq=FALSE)
      abline(v = median(postAR),
             col = "black",
             lwd = 3)
      
      upbound1<-min(postAR[postAR > quantile(postAR,prob=1-n1/100),])
      
      abline(v=upbound1,col=color,lty=2,lwd=3)
      
      lwrbound1<-max(postAR[postAR<quantile(postAR,prob=1-n2/100),])
      abline(v=lwrbound1,col=color,lty=2,lwd=3)
      
      
      legend(x = "topright", 
             c("Median",paste("50% Credibility Interval:",format(lwrbound1,big.mark=","), "-" ,format(upbound1,big.mark=","))),
             col =  c("black",color),lty=c(1,2),
             lwd = c(2,2))}
    
    else if (input$ModelType == "5 Year Autoregressive") {
      plot(hAR5, main=paste("Median Visitation Estimate = ", format(medAR5,big.mark = ",")),
           xlab="Number of Total Visitors", xlim=c(minAR5,maxAR5),ylab="Percentage of Samples",col=arcol,freq=FALSE)
      abline(v = median(postAR5),
             col = "black",
             lwd = 3)
      
      upbound1<-min(postAR5[postAR5 > quantile(postAR5,prob=1-n1/100),])
      
      abline(v=upbound1,col=color,lty=2,lwd=3)
      
      lwrbound1<-max(postAR5[postAR5<quantile(postAR5,prob=1-n2/100),])
      abline(v=lwrbound1,col=color,lty=2,lwd=3)
      
      
      legend(x = "topright", 
             c("Median",paste("50% Credibility Interval:",format(lwrbound1,big.mark=","), "-" ,format(upbound1,big.mark=","))),
             col =  c("black",color),lty=c(1,2),
             lwd = c(2,2))}
    
    
    
  })
  
  
  #Make Error Metrics Tab
  output$table <- DT::renderDataTable({
    DT::datatable(ErrorMetrics,extensions = 'Buttons' , options = list(dom='Bfrtip',buttons=c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  # Make All Data Tab
  output$data <- DT::renderDataTable({
    DT::datatable(Parks, extensions = 'Buttons' , options = list(dom='Bfrtip',buttons=c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
}

shinyApp(ui = ui, server = server)