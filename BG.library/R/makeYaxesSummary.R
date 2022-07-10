#'@title makeYaxesSummary
#'@description Set up y axes for summaryPlot_ly as main plot \\cr \\cr
#'@param addSetting character vector of settings to plot c("basal,"corrFactor","carbRatio")
#'@param settingOverlay TRUE/FALSE whether or not settings overlay main plot
#'@param percentSetting numeric value (0-100) for percentage of total plot to dedicate to setting subplot
#'@param addBarSub TRUE/FALSE whether barsubplot is to be included
#'@param addBG TRUE/FALSE whether BG values should be added to current plot
#'@param initYrange numeric vector for range of initial y axis on the left
#'@param yTitle character string for left yaxis title
#'@return `yaxisStr.list` named.list(yaxisStr,ay.list, xDomain)
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                 startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                 fromChange = TRUE,libraryPath = libraryPath)
#'
#'#set parameters
#'boxBar<-"bar"
#'sumFunc<-"mean"
#'plotSummary<-"BG.Reading..mg.dL."
#'addSetting<-c("basal","carbRatio")
#'settingOverlay<-FALSE
#'percentSetting<-30
#'addBarSub<-FALSE
#'addBG<-FALSE
#'
#'#subset settings
#'pumpSettings.list<-subsetSetting(data,libraryPath)
#'unPackList(lists = list(pumpSettings.list = pumpSettings.list),
#'           parentObj = list(NA)) 
#'
#'#set up data for summaryPlot_ly
#'dataOrig<-data
#'data$temp<-eval(parse(text = paste0("data$",plotSummary)))
#'data<-data[c("hour","temp")]
#'dataFormat<-data
#'
#'#set y axis parameters
#'initYrange<-c(0,450)
#'yTitle<-ifelse(sumFunc!="length",paste0(sumFunc,"_",plotSummary),paste0("number_",plotSummary))
#'
#'#format time in decimal hours for xaxis tick marks
#'xticks.list<-xTicks(data = dataOrig, startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1)
#'unPackList(lists = list(xticks.list = xticks.list),
#'           parentObj = list(NA)) 
#'
#'#get yaxis code string
#'yaxisStr.list<-makeYaxesSummary(addSetting, settingOverlay, percentSetting,addBarSub,addBG,
#'                                initYrange,yTitle)
#'
#'unPackList(lists = list(yaxisStr.list = yaxisStr.list),
#'           parentObj = list(NA)) 
#'unPackList(lists = list(ay.list = ay.list),
#'           parentObj = list(NA))
#'ay.list<-yaxisStr.list$ay.list
#'
#'#get xAxis str
#'xaxisStr<-makeXaxis(xDomain)
#'
#'#make title str
#'titleStr<-paste0(min(data$Date2)," -to- ",max(data$Date2))
#'
#'##make layoutstr
#'layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange = FALSE,stackedBar = "",
#'                      description = "",descInset = NA)
#'#initialize plot
#'p<-plot_ly()
#'#add layout
#'eval(parse(text = layoutStr))
#'#get formatted data
#'data<-dataFormat
#'
#'#summarize data
#'sumString<-paste0("as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(",
#'                  sumFunc,"),na.rm = TRUE))")
#'data<-eval(parse(text = sumString))
#'
#'#add bars to plot
#'p %>% add_trace(data = data, x = ~hour, y = ~temp,type = 'bar', 
#'                name = paste0(sumFunc,"_",plotSummary)) 


makeYaxesSummary<-function(addSetting, settingOverlay, percentSetting,addBarSub,addBG,
                       initYrange,yTitle){
  
  #set yDomain
  percentBar = "" #no bar subplot on summary plots
  addBolusType = "" #no bolus points added to summary plots
  domain.list<-makeYdomain(percentSetting,percentBar,addSetting,settingOverlay,addBarSub)
  unPackList(lists = list(domain.list = domain.list),
             parentObj = list(NA)) 
  
  #get number axes
  if (addSetting[1]!=""){
    numberAxes<-length(addSetting[addSetting!=""])
  }else{
    numberAxes<-1
  }
  
  #set position
  position<-1
  allPosition<-position
  
  #itialize plotly plot y axis
  yaxisStr<-paste0("yaxis = list(range = c(",paste(initYrange, collapse = ","),"), 
                                 ticks = 'outside', zeroline = FALSE,showline = TRUE,
                                 title = '",yTitle, "',
                                domain = c(",paste(yDomain1,collapse = ","),"))")
  
  
  
  #create y axes for pump settings
  settingAxis.list<-makeYaxesSetting(addSetting, settingOverlay, addBolusType,addBarSub,
                                     allPosition, position,numberAxes,yDomain2,yDomain3,yaxisStr)
  if (addSetting[1]!=""){
  unPackList(lists = list(settingAxis.list = settingAxis.list),
             parentObj = list(NA)) 
  unPackList(lists = list(ay.list = ay.list),
             parentObj = list(NA)) 
  }
  
  if(addBG){ #set up y axis if adding bg points to summary plot
    ayBG<-list(
      tickfont = list(color = I("black")),
      color = I("black"),
      overlaying = 'y',
      side = 'right',
      title = 'BG.Reading..mg.dL.' ,
      showgrid = FALSE,
      showline = TRUE,
      # tickprefix = " ",
      ticks = 'outside',
      anchor = 'free',
      position  = min(allPosition),
      range = c(0,450),
      domain = yDomain1,
      zeroline = FALSE)
    
    yStr<-"yaxis7=ayBG"
    
    yaxisStr<-paste0(yaxisStr,",",yStr)
  }
  
  #make list of all ay objects
  ay.list<-list()
  for (a in ls()[startsWith(ls(),"ay")]){
    eval(parse(text = paste0("ay.list$",a,"<-",a)))
  }
  
  #set x domain to min(allPOsition)
  xDomain<-min(allPosition) 
  xDomain<-paste0("domain = c(0,",xDomain,"),")
  
  
  #return axis string and ay objects
  yaxisStr.list<-named.list(yaxisStr,ay.list, xDomain)
  return(yaxisStr.list)
}
