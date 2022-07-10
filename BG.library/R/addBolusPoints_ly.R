#'@title addBolusPoints_ly
#'@description Adds Bolus values as scatter trace to plot_ly interactive plot \\cr \\cr
#'@param p current plot_ly plot
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param addBolusType character string vector of Bolus columns to add as scatter points 
#'addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U.")
#'@param pointSize scatter point size
#'@return `p` plot_ly interactive plot with BG values added if addBG
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                fromChange = TRUE,libraryPath = libraryPath)
#'p<-plot_ly()
#'addBolusPoints_ly(p,data addBolusType = c("Bolus.Volume.Delivered..U.",
#'                                             "BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U."))

addBolusPoints_ly<-function(p,data, addBolusType = "Bolus.Volume.Delivered..U.", 
                            pointSize = 10,startTime = "00:00",endTime = "23:00"){
if (addBolusType[1]!=""){#if add bolus type
  #set colors and shapes
  cls<-brewer.pal(length(addBolusType),"Set2")
  shapes<-c("circle","square","triangle-up")
  
  
  for (b in addBolusType){
    #color and shape
    cl<-cls[which(addBolusType==b)]    
    sh<-shapes[which(addBolusType==b)]
    
    #add temp column to data
    data$bolusType<-eval(parse(text = paste0("data$",b)))

    #get unique values
    NAMES<-c("dateTime","Date2","time2","hours","hour","bolusType")
    data2<-uniqueDateTime(data, NAMES, replaceNAs = FALSE,startTime = startTime,endTime = endTime,timeStep = "hour", period = 1)
    
    
    #set marker text
    markerText<-paste0("~paste('</br> Date: ',Date2,
                                            '</br> Time: ',time2,
                                            '</br> ",b," :',bolusType)")
    #add trace
    p <- p %>% add_trace( data = data2, x = ~hours, y = ~bolusType, 
                          type = "scatter", 
                          mode = "markers",
                          marker = list(symbol = sh,
                                        color = cl,
                                        size = pointSize,
                                        line = list(color = I("black"), width = 1)),
                          hoverinfo = 'text',
                          text = eval(parse(text = markerText)),
                          name = b,
                          yaxis = "y2")
    
  }#for each b
  }#if addBolus

return(p)
}#endFunction
