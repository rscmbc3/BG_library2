#'@title summaryLinePlot_ly
#'@description Adds summary glucose value lines (min, mean, and max) to plotly plot for column
#'defined in `plotSummary` \\cr \\cr
#'@param p current plot_ly plot
#'@param data data.frame with data to use for summary plot lines
#'@param plotSummary glucose values to summarise outputing min, mean, and max lines to plot
#'common options are 'BG.Reading..mg.dL.' or 'Sensor.Glucose..mg.dL.'
#'@return `p` plotly plot object
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                 startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                 fromChange = TRUE,libraryPath = libraryPath)
#'p<-plot_ly()
#'p<-addBGpoints_ly(p, data)
#'summaryLinePlot_ly(p, data, plotSummary = "Sensor.Glucose..mg.dL.")

summaryLinePlot_ly<-function(p, data, plotSummary){
  
  
  sumdata<-data[c("time2",plotSummary)]

  sumdata$time3<-as.POSIXct(round(as.POSIXct(sumdata$time2,format="%H:%M"),"hours"))

  sumdata<-sumdata[c("time3",plotSummary)]
  sumdata<-as.data.frame(sumdata %>% group_by(time3) %>% summarise_all(funs(min, mean, max),na.rm = TRUE))

  #round time to 0:00 that rounds to next day (i.e. closer to 24hrs and 23hrs)
  if (unique(sumdata$time3)[length(unique(sumdata$time3))]-unique(sumdata$time3)[1]==1){
    for (i in 1:nrow(sumdata)){
      if (sumdata$time3[i]==unique(sumdata$time3)[length(unique(sumdata$time3))]){
        sumdata$time3[i]<-unique(sumdata$time3)[1]
      }
    }
}

  sumdata$time3<-as.POSIXlt(sumdata$time3,format="%H:%M")
  sumdata$hours<- sumdata$time3$hour + sumdata$time3$min/60 
  sumdata<-sumdata[order(sumdata$time3),]

  
  p <- p %>% add_trace(data=sumdata, 
                       x=~hours, y=~min, mode='lines',
                       type="scatter", color=I("gray"), 
                       hoverinfo = 'text',
                       text = ~paste(
                                     '</br> Time: ',format(time3,"%H:%M"),
                                     paste0("'</br> ",plotSummary," min value : '"),min),
                       name = paste0("min_",plotSummary))
  p <- p %>% add_trace(data=sumdata, 
                       x=~hours, y=~mean, mode='lines',
                       type="scatter", color=I("black"), 
                       hoverinfo = 'text',
                       text = ~paste(
                         '</br> Time: ',format(time3,"%H:%M"),
                         paste0("'</br> ",plotSummary," mean value : '"),round(mean)),
                       name = paste0("mean_",plotSummary))
  p <- p %>% add_trace(data=sumdata, 
                       x=~hours, y=~max, mode='lines',
                       type="scatter", color=I("gray"), 
                       hoverinfo = 'text',
                       text = ~paste(
                         '</br> Time: ',format(time3,"%H:%M"),
                         paste0("'</br> ",plotSummary," max value : '"),max),
                       name = paste0("max_",plotSummary))
  return(p)
  
  
}#end function
  