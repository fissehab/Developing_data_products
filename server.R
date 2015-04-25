library(shiny)

library(chron)
library(RColorBrewer)
library(lattice)

library(ncdf)

library(maptools)
data(wrld_simpl)
library(fields)

library(rCharts)


pre.nc <- open.ncdf("data/cru_africaPre80_13.cdf")


temp.nc <- open.ncdf("data/cru_africaTemp80_13.cdf")

pre <- get.var.ncdf(pre.nc, "PRE") 
temp <- get.var.ncdf(temp.nc, "TEMP") 

pre.nc$dim$LON321_480$vals -> lon
pre.nc$dim$LAT121_240$vals -> lat
pre.nc$dim$TIME$vals -> time

close.ncdf(pre.nc)


close.ncdf(temp.nc)


shinyServer(function(input, output) {
  
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Temperature" = temp,
           "Rainfall" = pre)
  })

  
  output$distPlot <- renderPlot({
    dataset   <- datasetInput()
    
   
    
    todisp<-apply(dataset,c(1,2),mean)
    
    
    if(input$dataset=="Temperature"){
      title<-"Monthly Average Temperature (Degree C)"
      
      mypalette<-brewer.pal(9,"OrRd")
      
    }
    
    else
    {
    title<-"Monthly Average rainfall (mm)"
  
    mypalette<-brewer.pal(9,"BuGn")
    }
    
    
    
    
    
   
    image.plot(lon,lat,todisp, col = mypalette,
               xlab ="Longitude", ylab="Latitude",
               main =title)
    
    plot(wrld_simpl,add=TRUE)
    
  })
    
    
    
  v <- reactiveValues(
    click1 = NULL,  # Represents the first mouse click, if any
    range = NULL    # After two clicks, this stores the range of x
  )
  
  # Handle clicks on the plot
  observeEvent(input$plot_click, {
    if (is.null(v$click1)) {
      # We don't have a first click, so this is the first click
      v$click1 <- input$plot_click
    } else {
      # We already had a first click, so this is the second click.
      # Make a range from the previous click and this one.
      v$range <- c(v$click1$x,v$click1$y, input$plot_click$x,input$plot_click$y)
      # And clear the first click so the next click starts a new
      # range.
      v$click1 <- NULL
    }
  })
  
  observeEvent(input$reset, {
    # Reset both the range and the first click, if any.
    v$range <- NULL
    v$click1 <- NULL
  })
  
  
  
  output$rang <- renderText({
    
    round(as.numeric(v$range,0))
    
  })
   
  
  
  output$distPlot2 <- renderPlot({
    dataset   <- datasetInput()

      range1<-round(as.numeric(v$range),0)
      
      if (length(range1)>0){
        for (i in 1:4){
          if(range1[i]<=0 & i%%2==1){ range1[i]=(range1[i]+20)*2}
          else if(range1[i]<=0 & i%%2==0){ range1[i]=(range1[i]+30)*2}
          else{
            if(i%%2==0){
              range1[i]=range1[i]*2+60}
            else{range1[i]=range1[i]*2+40}
          }
        }
        
       xy<- range1
       
  clim1<-as.matrix(apply(dataset[xy[1]:xy[3],xy[2]:xy[4],],3,mean))
  
  month<-matrix(clim1,nrow=12,ncol=34) 
  
  monthly <-apply(month,1,mean)
  
  labels<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",sep = " ")
  
  par(mar = c(7, 4, 4, 2) + 0.1)
  
  if(input$dataset=="Temperature"){
    
    ylab1="Degree C"
    title='Monthly Average Temperature'
    
  }
  else {
    
    ylab1="mm"
    title='Monthly Average rainfall'
    
  }
  
  plot(monthly,type='o',col="blue",lwd=3,
       ylab=ylab1,main=title,
       xaxt = "n",  xlab =" ")
  
  axis(1, labels = FALSE)
  
    text(1:12, par("usr")[1] +(min(monthly)+max(monthly))/2, srt = 45, adj = 1,
         labels = labels, xpd = TRUE)
 
      }

  
  
  output$trendaf <- renderPlot({
    dataset   <- datasetInput()
    
    range1<-round(as.numeric(v$range),0)
    
    if (length(range1)>0){
      for (i in 1:4){
        if(range1[i]<=0 & i%%2==1){ range1[i]=(range1[i]+20)*2}
        else if(range1[i]<=0 & i%%2==0){ range1[i]=(range1[i]+30)*2}
        else{
          if(i%%2==0){
            range1[i]=range1[i]*2+60}
          else{range1[i]=range1[i]*2+40}
        }
      }
      
      xy<- range1
      
      clim1<-as.matrix(apply(dataset[xy[1]:xy[3],xy[2]:xy[4],],3,mean))
      
      month<-matrix(clim1,nrow=12,ncol=34) 
      
      annual <-apply(month,2,mean)
      
      years<-1980:2013
      
      fit<-lm(annual~years)
      
      
      par(mar = c(7, 4, 4, 2) + 0.1)
      
      if(input$dataset=="Temperature"){
        
        ylab1="Degree C"
        title='Monthly Average Temperature'
        
      }
      else {
        
        ylab1="mm"
        title='Monthly Average Rainfall'
        
      }
      
      plot(annual,type='l',col='blue',xaxt = "n",  xlab =" ", ylab=ylab1,main=title)
      lines(fit$fitted.values,type='l',col='red',xaxt = "n",  xlab =" ")
      
      years<-seq(1980,2013,3)
      at=seq(1,34,3)
      
      axis(1, at=at,las = 2, hadj = 0.9,labels=FALSE)
      
      legend(1,max(annual), c("Monthly Average","Trend"),
             lty=c(1,1),
             lwd=c(2.5,2.5),col=c("blue","red"))
      
      text(seq(1,34,3), par("usr")[1] +(min(annual)+max(annual))/2, srt = 45, adj = 1,
           labels = years, xpd = TRUE)
      
  
      
    }
    
    

  
  output$dat <- renderTable({
    
    dataset <- datasetInput()
    range2<-round(as.numeric(v$range),0)
    
    if (!is.null(range2)){
      for (i in 1:4){
        if(range2[i]<=0 & i%%2==1){ range2[i]=(range2[i]+20)*2}
        else if(range2[i]<=0 & i%%2==0){ range2[i]=(range2[i]+30)*2}
        else{
          if(i%%2==0){
            range2[i]=range2[i]*2+60}
          else{range2[i]=range2[i]*2+40}
        }
      }
      
      xy<- range2
    }
    
    
    clim1<-as.matrix(apply(dataset[xy[1]:xy[3],xy[2]:xy[4],],3,mean))
    
    
    labels<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",sep = " ")
    
    x<- apply(matrix(clim1,nrow=12,ncol=34),1,mean)
    
    y<-cbind(labels[1:12],round(x,3))
    
    
    if(input$dataset=="Temperature"){
      
      colnames(y)<-c("month", "Temperature")
      
    }
    
    else
    {
      colnames(y)<-c("month", "Rainfall ")
    }
    
    
    print.data.frame(data.frame(y))
   
  })
    
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("data_",Sys.Date(), sep=".")
    },
    
    content = function(file) {
      
      sep<-switch(input$type,"Excel (CSV)"="," ,"Text(TSV)"="\t","Text (Space Separated"=" ")
      write.table(datasetInput(), file,sep=sep)
        
      })
   
    } )
  
})

})



