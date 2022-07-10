#'@title addBGpoints_ly
#'@description Adds BG values as scatter trace to plot_ly interactive plot \\cr \\cr
#'@param p current plot_ly plot
#'@param data data.frame with BG values in BGvalue
#'@param addBG TRUE/FALSE whether BG values should be added to current plot
#'@param pointSize scatter point size will be reduced by pointSize/1.5 for BG values
#'@return `p` plot_ly interactive plot with BG values added if `addBG`
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                fromChange = TRUE,libraryPath = libraryPath)
#'p<-plot_ly()
#'addBGpoints_ly(p, data)

addBGpoints_ly<-function(p,data, yAxis = 'y', addBG = TRUE, pointSize = 10,startTime = "00:00",endTime = "23:00"){
  if (addBG){#add bG values
    NAMES<-c("dateTime","Date2","time2","hours","hour","BGvalue")
    data<-uniqueDateTime(data, NAMES, replaceNAs = FALSE,startTime = startTime,endTime = endTime, timeStep = "hour", period = 1)

    p <- p %>% add_trace( data = data, x = ~hours, y = ~BGvalue, 
                          type = "scatter", 
                          mode = "markers",
                          marker = list(
                            color = "black",
                            size = pointSize/1.5),
                          hoverinfo = 'text',
                          text = ~paste('</br> Date: ',Date2,
                                        '</br> Time: ',time2,
                                        '</br> BG value :',BGvalue),
                          name = "BGvalue",
                          yaxis = yAxis)
    
    
  }#if addBG
  return(p)
}#end function