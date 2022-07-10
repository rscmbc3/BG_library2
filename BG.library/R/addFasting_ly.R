#'@title addFasting_ly
#'@description Adds mean fasting line that extends from earliest fasting time to latest fasting 
#'time to plot_ly interactive plot with or without a text annotation (`addFastingAnnot`) 
#'printing the mean fasting value. \\cr \\cr
#'@param p current plot_ly plot
#'@param data data.frame with BG values in BGvalue
#'@param addFasting TRUE/FALSE whether mean fasting line should be added to current plot
#'@param addFastingAnnot TRUE/FALSE whether mean fasting text annotation should be added to current plot
#'@return `p` plot_ly interactive plot with mean fasting values added if `addFasting`
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                fromChange = TRUE,libraryPath = libraryPath)
#'p<-plot_ly()
#'p<-addBGpoints_ly(p, data)
#'addFasting_ly(p, data)


addFasting_ly<-function(p, data, addFasting = TRUE, addFastingAnnot = TRUE){

  
  if(addFasting){
    #subset out before 5am values
    data<-data[!is.na(data$BGvalue),]
    data<-data[which(data$hours>=5 & data$hours<=10),]
    
    #get fasting times
    fastingTimes<-as.data.frame(data %>% group_by(Date2) %>% summarise(hours = min(hours,na.rm = TRUE)))

    #merge with data
    data<-merge(data,fastingTimes, by= c("Date2","hours"))
    
    
    #calculate mean fasting value and replicate for length unique(hours)
    meanFasting<-as.data.frame(data %>% group_by(Date2, hours) %>% summarise(BGvalue = max(BGvalue,na.rm = TRUE)))
    meanFasting<-meanFasting[,names(meanFasting) %in% c("hours","BGvalue")]
    meanFasting$value<-rep(mean(meanFasting$BGvalue),nrow(meanFasting))


    #add trace
    lineText<-paste0("Mean Fasting BG = ",round(unique(meanFasting$value)))
    p <- p %>% add_trace(data = meanFasting, x = ~hours, y = ~value,
                         type = "scatter", 
                         mode = "lines",
                         color = I("magenta"),
                         hoverinfo = 'text',
                         text = lineText,
                         name = "Mean Fasting BG")
   
    if(addFastingAnnot){
    #add_annotation
    p <- p %>% add_annotations(x = max(meanFasting$hours),
                    y = unique(meanFasting$value),
                    text = lineText,
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    arrowhead = 2,
                    arrowsize = 1,
                    ax = 20,
                    ay = -40)
    }
    
  }#if addFasting
  return(p)
  
}