library(shiny)

shinyServer(function(input, output) {
 
  ErrorMetrics <- read_csv("./Error_Metrics_for_app - Sheet1.csv")
  Full_GL_Results <- read_csv("./Full_GL_Results.csv")
  Full_GL_Results$X1<-NULL
  Full_AR5_Results <- read_csv("./Full_AR5_Results.csv")
  Full_AR5_Results$X1<-NULL
  load(file="./GLMall.rda")
  load(file="./AR5ALL.rda")
  Parks<- read_csv("Yearly_Visitation_Data.csv")
  Parks<-Parks[Parks$Park != "NPSA",]
  Names<-unique(Parks$Park)
  
  Codes<-read_csv("./Unit_codes_with_pop - Sheet1 (1).csv")
  #Codes<-Codes[,-1]
  
  tsdfAR5<-data.frame(matrix(nrow=5*length(Names), ncol=8))
  colnames(tsdfAR5) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
  tsdfAR5$Year=rep(2008:2012, 57)
  tsdfAR5$Park=rep(Names, each=5)
  tsdfAR5$Visitation=Parks[Parks$Year==2008:2012,]$Visitation
  tsdfAR5<-rbind(tsdfAR5,Full_AR5_Results)
  
  tsdfGL<-data.frame(matrix(nrow=5*length(Names), ncol=8))
  colnames(tsdfGL) <- c("Median", "LowerBound25","LowerBound50","UpperBound25","UpperBound50","Visitation","Year","Park")
  tsdfGL$Year=rep(2008:2012, 57)
  tsdfGL$Park=rep(Names, each=5)
  tsdfGL$Visitation=Parks[Parks$Year==2008:2012,]$Visitation
  tsdfGL<-rbind(tsdfGL,Full_GL_Results)
  
  output$a5R2<-renderPlotly({
    
    arcolinner<-rgb(246/255,84/255,106/255,alpha=0.6)
    arcolouter<-rgb(169/255, 24/255, 24/255,alpha=0.6)
    
    xax<-list(
      title = "Observed Visitation")
    
    yax<-list(
      title = "Median Visitation Estimate",range = c(-1000000, 25000000))
    
   plot_ly(
      Full_AR5_Results,x = ~Visitation, y = ~Median,
      marker=list(size = 20,
                  color = arcolinner,
                  line = list(color =arcolouter,
                              width = 3)),
      text = ~paste("Park: ", Park, "<br>Year: ", Year)) %>%
      layout(title="Autoregressive Forecast Accuracy", xaxis=xax, yaxis=yax)
     
    
  })
  
  output$glR2<-renderPlotly({ 
    
    gocolinner<-rgb(158/255,154/255,200/255,alpha=0.6)
    gocolouter<-rgb(84/255,39/255,143/255,alpha=0.6)
    
    xax<-list(
      title = "Observed Visitation")
    
    yax<-list(
      title = "Median Visitation Estimate",range = c(-1000000, 25000000))
    
    
    plot_ly(
      Full_GL_Results,x = ~Visitation, y = ~Median,
      marker=list(size = 20,
                  color = gocolinner,
                  line = list(color =gocolouter,
                              width = 3)),
      text = ~paste("Park: ", Park, "<br>Year: ", Year)) %>%
      layout(title="Google Trends Forecast Accuracy",xaxis=xax, yaxis=yax)
    
  })
  
  
  
  
  output$myplot <- renderPlot({
    
    minnimum<-(min(Full_GL_Results[Full_GL_Results$Park==input$Type,]$LowerBound50)/1.15)
    maximum<-  (max(Full_AR5_Results[Full_AR5_Results$Park==input$Type,]$UpperBound50)*1.15)
    
    
    
    p2<-(ggplot(tsdfGL[tsdfGL$Park==input$Type,], aes(x=Year))+
           geom_ribbon(aes(ymin=LowerBound50, ymax=UpperBound50,fill="#9e9ac8"),alpha=0.6)+
           geom_ribbon(aes(ymin=LowerBound25, ymax=UpperBound25,fill="#54278f"),alpha=0.6)+
           geom_line(aes(y=Visitation, x=Year,color="black"), linetype="dashed" ,size=1.5)+
           labs(title="Google Trends Forecast",y=NULL,
                x=NULL)+
           scale_y_continuous(labels = scales::comma,limits=c(minnimum, maximum))+
           scale_x_discrete(limits=(2008:2017))+
           theme(axis.text.x = element_text( vjust=0.5, size=22, angle=0), 
                 axis.text.y = element_text(vjust=0.5, size=22, angle=0),
                 plot.title=element_text(size=20, face="bold"),
                 panel.border = element_blank(), axis.line = element_line(),
                 panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                 axis.title=element_text(size=20,face="bold"), legend.position = "bottom", 
                 legend.text = element_text(size=15),legend.key = element_rect(size = 5, color = 'white'),
                 legend.key.size = unit(1.2, 'lines'))+
           scale_fill_identity( guide = 'legend',name=NULL, labels = c('25% C.I.     ', "50% C.I.")) +
           scale_colour_manual(name = NULL, values =c('black'='black','#6CC4EE'='#6CC4EE',"red"="red"), 
                               labels = c("Observed Visitation  ","Observed Visitation","Autoregressive Model")))
    
    p3<-(ggplot(tsdfAR5[tsdfAR5$Park==input$Type,], aes(x=Year))+
           geom_ribbon(aes(ymin=LowerBound50, ymax=UpperBound50,fill="#f6546a"),alpha=0.6)+
           geom_ribbon(aes(ymin=LowerBound25, ymax=UpperBound25,fill="#a91818"),alpha=0.6)+
           geom_line(aes(y=Visitation, x=Year,color="black"), linetype="dashed" ,size=1.5)+
           labs(title="Autoregressive Forecast",y=NULL,
                x=NULL)+
           scale_y_continuous(labels = scales::comma,limits=c(minnimum, maximum))+
           scale_x_discrete(limits=(2008:2017))+
           theme(axis.text.x = element_text( vjust=0.5, size=22, angle=0), 
                 axis.text.y = element_text(vjust=0.5, size=22, angle=0),
                 plot.title=element_text(size=20, face="bold"),
                 panel.border = element_blank(), axis.line = element_line(),
                 panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                 axis.title=element_text(size=20,face="bold"), legend.position = "bottom", 
                 legend.text = element_text(size=15),legend.key = element_rect(size = 5, color = 'white'),
                 legend.key.size = unit(1.2, 'lines'))+
           scale_fill_identity( guide = 'legend',name=NULL, labels = c('25% C.I.     ', "50% C.I.")) +
           scale_colour_manual(name = NULL, values =c('black'='black','#6CC4EE'='#6CC4EE',"red"="red"), 
                               labels = c("Observed Visitation  ","Observed Visitation","Autoregressive Model")))
    
    
    
    label = textGrob("Visitors", rot = 90, vjust = 0.5 ,gp = gpar(fontface = "bold", cex = 2))
    label2 = textGrob("Year", rot = 0, vjust = 0.5 ,gp = gpar(fontface = "bold", cex = 2))
    legend = gtable_filter(ggplotGrob(p2), "guide-box")
    
    
    
    
    
    
    
    
    
    grid.arrange( arrangeGrob(p2 ,p3,nrow=2), left=label,bottom=label2)})
   
  
  
  
  #Make Forecast Plots
  output$Forecast<- renderPlot({
    color=rgb(0,0,0,alpha=0.3)
    
    arcol<-rgb(169/255, 24/255, 24/255,alpha=0.6)
    gocol<-rgb(84/255,39/255,143/255,alpha=0.6)
    
    
    n1 <- 25 
    n2<-75
    
    options(scipen=10)
    
  
    
    
    
    postGL<-(posterior_predict(
      GLMall,data.frame(Lag12G=input$Lag12G,
                        Park=input$"Park"),draws=1000))
    
    hGL = hist(postGL,breaks=50,plot=FALSE) 
    hGL$density = hGL$counts/sum(hGL$counts)*100
    
    postAR5<-(posterior_predict(
      AR5ALL,data.frame(VisLag1=input$a5Vis1,
                        VisLag2=input$a5Vis2,
                        VisLag3=input$a5Vis3,
                        VisLag4=input$a5Vis4,
                        VisLag5=input$a5Vis5,
                        Park=input$"Park"),draws=1000))
    
    hAR5 = hist(postAR5,breaks=50,plot=FALSE) 
    hAR5$density = hAR5$counts/sum(hAR5$counts)*100
    
  
    medGL<-round(median(postGL),2)
    medAR5<-round(median(postAR5),2)
    
    minGL<-min(postGL)
    maxGL<-max(postGL)
    minAR5<-min(postAR5)
    maxAR5<-max(postAR5)
    
  
    
    if (input$ModelType == "Google Trends Forecast") {
      plot(hGL, main=paste("Median Visitation Estimate = ", format(medGL,big.mark = ",")),
           xlab="Number of Total Visitors", xlim=c(minGL,maxGL),ylab="Percentage of Samples",col=gocol,freq=FALSE, adj = 0.965)
      abline(v = median(postGL),
             col = "black",
             lwd = 3)
      
      
      upbound1<-min(postGL[postGL > quantile(postGL,prob=1-n1/100),])
      
      abline(v=upbound1,col=color,lty=2,lwd=3)
      
      lwrbound1<-max(postGL[postGL<quantile(postGL,prob=1-n2/100),])
      abline(v=lwrbound1,col=color,lty=2,lwd=3)
      
      
      legend(x = "topright", 
             c("Median",paste("50% Credibility Interval:",format(lwrbound1,big.mark=","), "-" ,format(upbound1,big.mark=","))),
             col =  c("black",color),lty=c(1,2),
             lwd = c(2,2))}
    
    else if (input$ModelType == "Autoregressive Forecast") {
      plot(hAR5, main=paste("Median Visitation Estimate = ", format(medAR5,big.mark = ",")),
           xlab="Number of Total Visitors", xlim=c(minAR5,maxAR5),ylab="Percentage of Samples",col=arcol,freq=FALSE, adj = 0.965)
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
    DT::datatable(ErrorMetrics,extensions = c('Buttons','Scroller') , options = list(dom='Bfrtip',buttons=c('copy', 'csv', 'excel', 'pdf', 'print'), deferRender = TRUE,
                                                                                     scrollX=TRUE,pageLength=20,
                                                                                     scrollY = 200,
                                                                                     scroller = TRUE))
  })
  
  # Make All Data Tab
  output$data <- DT::renderDataTable({
    DT::datatable(Parks, extensions = c('Buttons','Scroller') , options = list(dom='Bfrtip',buttons=c('copy', 'csv', 'excel', 'pdf', 'print'), deferRender = TRUE,
                                                                               scrollX=TRUE,pageLength=20,
                                                                               scrollY = 200,
                                                                               scroller = TRUE))
  })
  
  #Make unit code key
  output$Codes <- DT::renderDataTable({
    DT::datatable(Codes,extensions = 'Buttons' , options = list(dom='Bfrtip',buttons=c('copy', 'csv', 'excel', 'pdf', 'print'), deferRender = TRUE,
                                                                                    pageLength=60
                                                                                     ))
  })
})
