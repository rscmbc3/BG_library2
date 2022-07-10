#'@title addStackbar_ly
#'@description Adds stacked bars (insulin, BG, or SG) to plotly interactive plot. \\cr \\cr
#'@param p current plot_ly plot
#'@param data data.frame with data to use for stacked bars
#'@param cls character vector of colors (i.e. `cls<-c("blue","white","red","darkred")`)
#'@param timeStep character string indicating the time step to aggregate data, possible values
#'include c("hour","day")
#'@return `p` plot_ly interactive plot with stacked bars added
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                fromChange = TRUE,libraryPath = libraryPath)
#'data<-data[c("hour","BG.Reading..mg.dL.")]
#'data$veryHigh<-ifelse(data$BG.Reading..mg.dL.>240,1,0)
#'data$high<-ifelse(data$BG.Reading..mg.dL.>150 & data$BG.Reading..mg.dL.<=240,1,0)
#'data$good<-ifelse(data$BG.Reading..mg.dL.>=80 & data$BG.Reading..mg.dL.<=150,1,0)
#'data$low<-ifelse(data$BG.Reading..mg.dL.<80,1,0)
#'data<-data[,names(data)!="BG.Reading..mg.dL."]
#'data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(sum),na.rm=TRUE))
#'orderVector<-c("hour","low","good","high","veryHigh")
#'data<-data[,match(names(data),orderVector)]
#'cls<-c("blue","white","red","darkred")
#'p<-plot_ly()
#'p<- p %>% layout(barmode = 'stack')
#'addStackbar_ly(p,data,cls,timeStep = "hour") 

addStackbar_ly<-function(p,data,cls, timeStep){
  if (timeStep=="hour"){
    for (s in names(data)[2:length(data)]){
    data$temp<-eval(parse(text = paste0("data$`",s,"`")))
    p <- p %>% add_trace(data = data, x = ~hour, y = ~temp,type = "bar",
                         name = s, 
                         marker = list(color = cls[which(names(data)==s)-1],
                                       line = list(color = I("black"),
                                                   width = 1.5)))
    
    
  }#for each type of insulin 
  }else if (timeStep=="day"){
    for (s in names(data)[2:length(data)]){
      data$temp<-eval(parse(text = paste0("data$`",s,"`")))
      p <- p %>% add_trace(data = data, x = ~Date2, y = ~temp,type = "bar",
                           name = s, 
                           marker = list(color = cls[which(names(data)==s)-1],
                                         line = list(color = I("black"),
                                                     width = 1.5)))
      
      
    }#for each type of insulin 
  }
 
  return(p)
}