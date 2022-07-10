#'@title addPumpSetting_ly
#'@description Adds pump settings as line trace to plot_ly interactive plot, either overlaying or subplot \\cr \\cr
#'@param p current plot_ly plot
#'@param addSetting character vector of settings that should be added to current plot. Options
#'include 'basal', 'carbRatio', 'corrFactor', or ''
#'@param settingOverlay TRUE/FALSE whether settings should overlay the data or 
#'if FALSE plot settings as subplot below data
#'@param startTime character string of beginning time for plot (typically startTime = "00:00)
#'@param endTime character string of ending time for plot (typically endTime = "23:00)
#'@param ay.list list of yaxis specifications for each setting, 
#'settings are given y axis of y3, y4, y5 and ay3, ay4, ay5 respectively
#'@param xticks numeric vector of tickmarks for the xaxis
#'@param yaxisStr character string for layout of all y axes.
#'@param legendInset numeric value specifying how far below the plot the legend should be placed
#'@return `p` plot_ly interactive plot with settings plotted as lines
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                fromChange = TRUE,libraryPath = libraryPath)
#'#output as 'plot_ly'
#'#set parameters
#'addSetting<-c("basal","corrFactor","carbRatio")
#'settingOverlay<- FALSE
#'legendInset<--0.2
#'#format time in decimal hours
#'xticks.list<-xTicks(data, startTime = "00:00",endTime = "23:00",
#'                    timeStep = "hour",period = 1)
#'unPackList(lists = list(xticks.list = xticks.list),
#'           parentObj = list(NA)) 
#'#make y axis str
#'yaxisStr.list<-makeYaxes(addBolusType = "", addSetting,settingOverlay,
#'                         percentSetting = 30,addBarSub = FALSE,percentBar = NA,yTitle = "")
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


addPumpSetting_ly<-function(p, addSetting = c("basal","corrFactor","carbRatio"),
                            settingOverlay = FALSE, 
                            startTime = "00:00",endTime = "23:00",
                            startDate = NA, endDate = NA,
                            ay.list,xticks,yaxisStr,
                            legendInset = -0.2){
  
  #if adding settings to plot
  if(addSetting[1]!=""){
    #get pumpSettings
    pumpSettings.list<-makePumpSettings(libraryPath)
    unPackList(lists = list(pumpSettings.list = pumpSettings.list),
               parentObj = list(NA)) 
    
    setColors<-c("green","blue","red")
    
    #unpack ay.list
    unPackList(lists = list(ay.list = ay.list),
               parentObj = list(NA))
    
    #set index for axes
    for (s in 1:length(addSetting)){
      if (s==1){
        i<-3
      }else{
        i<-i+1
      }
    } 
      
      #for each setting to plot 
      for (s in 1:length(addSetting)){
        #set up temporary data
        settingData<-eval(parse(text = addSetting[s]))
        
        if (is.na(startDate) & is.na(endDate)){
        settingData$rate<-settingData[[length(settingData)]]
        }else if (is.na(startDate)){
          endDate<-as.POSIXlt(endDate, format = "%Y-%m-%d")
          startDate<-endDate
          allStartDates<-names(settingData[2:length(settingData)])
          allStartDates<-as.POSIXlt(gsub("X","",allStartDates), format  = "%m.%d.%Y")
          substartDate<-which.min(abs(startDate-allStartDates))
          substartDate<-ifelse(allStartDates[substartDate]<=startDate,substartDate,substartDate+1)
          
          if (endDate>=max(allStartDates)){
            settingData$rate<-settingData[[length(settingData)]]
          }else{
          settingData$rate<-settingData[substartDate]
          }
        }else if (is.na(endDate)){
          startDate<-as.POSIXlt(startDate, format = "%Y-%m-%d")
          allStartDates<-names(settingData[2:length(settingData)])
          allStartDates<-as.POSIXlt(gsub("X","",allStartDates), format  = "%m.%d.%Y")
          substartDate<-which.min(abs(startDate-allStartDates))
          substartDate<-ifelse(allStartDates[substartDate]<=startDate,substartDate,substartDate+1)
          
          allRates<-settingData[substartDate:length(settingData)]
          if (startDate>=max(allStartDates)){
            settingData$rate<-settingData[[length(settingData)]]
          }else{
          settingData$rate<-rowMeans(allRates)
          }
          
        }else{
          endDate<-as.POSIXlt(endDate, format = "%Y-%m-%d")
          startDate<-as.POSIXlt(startDate, format = "%Y-%m-%d")
          
          allStartDates<-names(settingData[2:length(settingData)])
          allStartDates<-as.POSIXlt(gsub("X","",allStartDates), format  = "%m.%d.%Y")
          substartDate<-which.min(abs(startDate-allStartDates))
          substartDate<-ifelse(allStartDates[substartDate]<=startDate,substartDate,substartDate+1)
         
          subendDate<-which.min(abs(endDate-allStartDates))
          subendDate<-ifelse(allStartDates[subendDate]<=endDate,subendDate-1,subendDate)
          
          allRates<-settingData[(substartDate+1):(subendDate+1)]
          
          if (endDate>=max(allStartDates) & startDate>=max(allStartDates)){
            settingData$rate<-settingData[[length(settingData)]]
          }else{
          settingData$rate<-rowMeans(allRates)
          }
        }
        
        #format time
        settingData$time2<-as.POSIXlt(settingData$time,format="%H:%M")
        settingData$hours<- settingData$time2$hour + settingData$time2$min/60 
        
        #save index
        if (s==1){
          i<-3
        }else{
          i<-i+1
        }
        
        #set lineText for hover    
        lineText<-paste0("~paste('</br> Time: ',time2,
                         '</br> ",addSetting[s]," :',rate)")
        
        #add line trace
        p <- p %>% add_trace( data = settingData, x = ~hours, y = ~rate, 
                              type = "scatter", 
                              mode = "lines",
                              color = I(setColors[s]),
                              hoverinfo = 'text',
                              text = eval(parse(text = lineText)),
                              name = addSetting[s],
                              yaxis = paste0("y",i))
        
        
      }#for each setting
    }#if add setting
    return(p)
  }#end function