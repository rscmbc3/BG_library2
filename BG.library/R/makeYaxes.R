#'@title makeYaxes
#'@description creates lists to execute for plot_ly layout of y axes customized according to 
#'data selection with multiple axes and scales.  \\cr \\cr
#'@param addBolusType character string vector of Bolus columns to add as scatter points 
#'addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U.")
#'@param addSetting character vector of settings that should be added to current plot. Options
#'include 'basal', 'carbRatio', 'corrFactor', or ''
#'@param settingOverlay TRUE/FALSE whether settings should overlay the data or 
#'if FALSE plot settings as subplot below data
#'@param percentSetting numeric percentage of plotting area to dedicate to setting subplot (0-100)
#'@param addBarSub TRUE/FALSE indicating whether subplot of mean carb intake per hour is included
#'@param percentBar numeric percentage of plotting area to dedicate to carb intake bar subplot (0-100)
#'@param yTitle character string for left yaxis title
#'@return `yaxisStr.list` `named.list(yaxisStr,ay.list, xDomain)` to execute as part of plot_ly layout 
#'of the yaxes
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                 startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                 fromChange = TRUE,libraryPath = libraryPath)
#'#set paramters
#'addSetting<-c("basal","carbRatio")
#'settingOverlay<- TRUE
#'legendInset<--0.2
#'
#'#format time in decimal hours
#'xticks.list<-xTicks(data, startTime = "00:00",endTime = "23:00",
#'                    timeStep = "hour",period = 1)
#'unPackList(lists = list(xticks.list = xticks.list),
#'           parentObj = list(NA)) 
#'#make y axis str
#'yaxisStr.list<-makeYaxes(addBolusType = "", addSetting,settingOverlay,
#'                         percentSetting =NA,addBarSub = FALSE,percentBar = NA,yTitle = "")
#'
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
#'addPumpSetting_ly(p,addSetting, settingOverlay,startTime,endTime,
#'                  ay.list,xticks,yaxisStr,legendInset)



makeYaxes<-function(addBolusType, addSetting, settingOverlay, percentSetting,addBarSub,percentBar,yTitle){

  #set yDomain
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
yaxisStr<-paste0("yaxis = list(range = c(0,450), ticks = 'outside', zeroline = FALSE,showline = TRUE,
                               title = '",yTitle, "',
                               domain = c(",paste(yDomain1,collapse = ","),"))")

#add to yaxisstr
if (addBolusType[1]!=""){
  numberAxes<-numberAxes+1
  position<-position - 0.1*(numberAxes-1)
  allPosition<-c(allPosition, position)

  ayInsulin <- list(
    tickfont = list(color = I("black")),
    overlaying = 'y',
    side = 'right',
    title = "Insulin Units" ,
    showline = TRUE,
    anchor = 'free',
    position = position,
    showgrid = FALSE,
    zeroline = FALSE,
   ticks = 'outside',
   domain = yDomain1,
    rangemode =  'tozero')
  
  
  yaxisStr<-paste0(yaxisStr,",",
                   "yaxis2 = ayInsulin")
}#end addBolus for y axis 

#create y axes for pump settings
settingAxis.list<-makeYaxesSetting(addSetting, settingOverlay, addBolusType,addBarSub,
                                   allPosition, position,numberAxes,yDomain2,yDomain3,yaxisStr)
if (addSetting[1]!=""){
unPackList(lists = list(settingAxis.list = settingAxis.list),
           parentObj = list(NA)) 
unPackList(lists = list(ay.list = ay.list),
           parentObj = list(NA)) 
}

if (addBarSub){#if add bar
  numberAxes<-numberAxes+1
  position<-position - 0.1*(numberAxes-1)
  allPosition<-c(allPosition, position)
  
  ayCarb <- list(
    tickfont = list(color = I("blue")),
    #overlaying = 'y',
    side = 'right',
    title = "BWZ.Carb.Input..grams." ,
    showline = TRUE,
    anchor = 'free',
    position = position,
    showgrid = FALSE,
    zeroline = TRUE,
    ticks = 'outside',
    domain = yDomain2,
    rangemode =  'tozero')
  
  
  yaxisStr<-paste0(yaxisStr,",",
                   "yaxis6 = ayCarb")
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
