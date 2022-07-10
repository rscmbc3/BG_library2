#'@title barSubPlot_ly
#'@description Adds mean per hour carb intake subplot to plotly interactive plot. \\cr \\cr
#'@param p current plot_ly plot
#'@param data data.frame with data to use for stacked bars
#'@param addBarSub TRUE/FALSE whether or not subplot should be added to `p`
#'#'@param timeStep character string indicating the time step to aggregate data, possible values
#'include c("hour","day")
#'@param period numeric value indicating number of `timeSteps` to aggregate into single step
#'for example : `timeStep = 'hour'`  and `period = 3` outputs plots with tick marks every 3 hours.
#'@return `p` plot_ly interactive plot with carb instake subplot
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                fromChange = TRUE,libraryPath = libraryPath)
#'#set paramters
#'addSetting<-""
#'settingOverlay<- FALSE
#'legendInset<--0.2
#'addBarSub<-TRUE
#'#format time in decimal hours
#'xticks.list<-xTicks(data, startTime = "00:00",endTime = "23:00",
#'                    timeStep = "hour",period = 1)
#'unPackList(lists = list(xticks.list = xticks.list),
#'         parentObj = list(NA)) 
#'#make y axis str
#'yaxisStr.list<-makeYaxes(addBolusType = "", addSetting,settingOverlay,
#'                         percentSetting =NA,addBarSub,percentBar = 30,yTitle = "")

#'unPackList(lists = list(yaxisStr.list = yaxisStr.list),
#'           parentObj = list(NA)) 
#'unPackList(lists = list(ay.list = ay.list),
#'           parentObj = list(NA))
#'ay.list<-yaxisStr.list$ay.list
#'#get xAxis str
#'xaxisStr<-makeXaxis(xDomain)
#'
#'#make title str
#'titleStr<-paste0(min(data$Date2)," -to- ",max(data$Date2))
#'
#'##make layoutstr
#'layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange = FALSE,
#'                      description = "",descInset =NA)
#'p<-plot_ly()
#'#add layout
#'eval(parse(text = layoutStr))
#'p<-addBGpoints_ly(p, data)
#'barSubPlot_ly(p, data, addBarSub,timeStep = "hour", period = 1)


barSubPlot_ly<-function(p,data,addBarSub, timeStep, period){
  if (addBarSub){
  #only look at pump data
  data<-data[is.na(data$SGvalue),]
  
  #format data
  NAMES<-c("dateTime","Date2","hours","hour","BWZ.Carb.Input..grams.")
  data<-uniqueDateTime(data, NAMES, replaceNAs = TRUE,startTime = startTime,endTime = endTime, timeStep = timeStep, period = period)
  
  #get only relavant columns
  data<-data[,names(data) %in% c("hour","BWZ.Carb.Input..grams.")]
  
  #group-by and mean
  data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(mean),na.rm = TRUE))
  
  #create plot
  p <- p %>% add_bars(data = data, x = ~hour, y = ~BWZ.Carb.Input..grams., 
                      name = "mean BWZ.Carb.Input..grams.",
                      yaxis = "y6")
  
  return(p)
  }else{#not adding plot return p as is
    return(p)
  }
}