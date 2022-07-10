#'@title plotLine_ly
#'@description Creates interactive plotly scatter and line plots \\cr \\cr
#'@param data data.frame with BG values in BGvalue and SG values in SGvalue
#'@param scatterOnly TRUE/FALSE indicating whether only points with no lines are to be plotted
#'@param addBG TRUE/FALSE whether BG values should be added to current plot
#'@param addPercentBG character vector of groups to include (c("low","good","high","very high"))
#'@param addPercentType character string of column name with values to group (i.e. "BGvalue")
#'@param addBolusType character string vector of Bolus columns to add as scatter points 
#'addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U.")
#'@param plotSummary glucose values to summarise outputing min, mean, and max lines to plot
#'common options are 'BGvalue' or 'SGvalue'
#'@param addSetting character vector of settings that should be added to current plot. Options
#'include 'basal', 'carbRatio', 'corrFactor', or ''
#'@param settingOverlay TRUE/FALSE whether settings should overlay the data or 
#'if FALSE plot settings as subplot below data
#'@param percentSetting numeric percentage of plotting area to dedicate to setting subplot (0-100)
#'@param addBarSub  TRUE/FALSE indicating whether subplot of mean carb intake per hour is included
#'@param percentBar numeric percentage of plotting area to dedicate to carb intake bar subplot (0-100)
#'@param addGoodRange TRUE/FALSE indicating whether shaded polygon for good BG range is plotted
#'@param addFasting TRUE/FALSE whether mean fasting line should be added to current plot
#'@param addFastingAnnot TRUE/FALSE whether mean fasting text annotation should be added to current plot
#'@param fromChange TRUE/FALSE indicates whether data should be subset with the ealiest date
#'as the most recent pump settings change.  This setting overrides all other date subsetting 
#'parameters, and must be set to `FALSE` to apply other parameter settings (i.e. `numberDays`,
#'`startDate`, and `endDate`)
#'@param numberDays numeric value indicating number of days of data to include.  This parameter will 
#'override `startDate` and `endDate` unless it is set to NA.  The `fromChange` parameter will override 
#'all other parameters that subset the data by date.
#'@param startDate Earliest date included in data.  This setting will only be applied 
#'if `numberDays = NA` and `fromChange = FALSE`
#'@param endDate Latest date included in data.  This setting will only be applied 
#'if `numberDays = NA` and `fromChange = FALSE`
#'@param startTime character string of beginning time for plot (typically startTime = "00:00)
#'@param endTime character string of ending time for plot (typically endTime = "23:00)
#'@param timeStep character string indicating the time step to aggregate data, possible values
#'include c("hour","day")
#'@param period numeric value indicating number of `timeSteps` to aggregate into single step
#'for example : `timeStep = 'hour'`  and `period = 3` outputs plots with tick marks every 3 hours.
#'@param filterCond character string of R syntax to be applied to filter the data, 
#'example `data[data$BGvalue>150 & !is.na(data$BGvalue),]`
#'@param colorPalleteDaily character string color pallete for daily sensor lines
#'@param pointSize scatter point size will be reduced by pointSize/1.5 for BG values
#'@param legendInset numeric value specifying how far below the plot the legend should be placed
#'@param description character string plot description to be output as part of plot
#'@param descInset numeric value to place description below plot
#'@param libraryPath character string path to BG.library code 
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'#line plot
#'plotLine_ly(data,addBolusType = "Bolus.Volume.Delivered..U.",
#'            numberDays = 5,libraryPath = libraryPath)

plotLine_ly<-function(data,
                      scatterOnly = FALSE,addBG = TRUE, 
                      addPercentBG = c("low","good","high","very high"),
                      addPercentType = "BGvalue",
                      addBolusType = "Bolus.Volume.Delivered..U.",
                      plotSummary = "SGvalue",
                      addSetting ="",settingOverlay = FALSE,percentSetting = 30,
                      addBarSub = TRUE, percentBar = 30,
                      addGoodRange = TRUE,addFasting = TRUE,addFastingAnnot = TRUE,
                      fromChange = TRUE, numberDays = NA, startDate = NA, 
                      endDate = NA,removeDates = NA,
                      startTime = "00:00", endTime = "23:00",
                      timeStep = "hour",period = 1,filterCond = "",
                      colorPalleteDaily = "rainbow", pointSize = 10,
                      legendInset = -0.2,description = "",descInset = -0.15,
                      libraryPath){
  

  #subset data by date and filterCond
  data<-subsetData(data,numberDays = numberDays,startDate = startDate,
                   endDate = endDate,filterCond = filterCond,
                   startTime = startTime, endTime = endTime,
                   timeStep = timeStep,period = period, fromChange = fromChange, 
                   removeDates = removeDates,libraryPath =libraryPath)

  #subset settings
  pumpSettings.list<-subsetSetting(data,libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  #if filtered data exists
  if(nrow(data)!=0){
    
    
    #format time in decimal hours
    xticks.list<-xTicks(data, startTime,endTime,timeStep,period)
    unPackList(lists = list(xticks.list = xticks.list),
               parentObj = list(NA)) 
    
    #get yaxis code string
    yaxisStr.list<-makeYaxes(addBolusType, addSetting,settingOverlay,
                             percentSetting,addBarSub,percentBar,yTitle = "")

    unPackList(lists = list(yaxisStr.list = yaxisStr.list),
               parentObj = list(NA)) 
    unPackList(lists = list(ay.list = ay.list),
               parentObj = list(NA))
    ay.list<-yaxisStr.list$ay.list
    

    #get xAxis str
    xaxisStr<-makeXaxis(xDomain)
    
    #make title str
    titleStr<-paste0(min(data$Date2, na.rm = TRUE)," -to- ",max(data$Date2, na.rm = TRUE))

    ##make layoutstr
    layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange,
                          description = description,descInset = descInset)

    #initialize plot
    p<-plot_ly(data)
    #add layout
    eval(parse(text = layoutStr))
    

    if (plotSummary!="SGvalue" & !scatterOnly){#daily sensor data
        if (is.na(numberDays)){
        numberDays<-as.numeric(max(data$Date2, na.rm = TRUE)-min(data$Date2, na.rm = TRUE))
      }
      #daily colors
      eval(parse(text = paste0("cl <- ",colorPalleteDaily,"(numberDays)")))
    
      
      for (i in 1:numberDays){
        p <- p %>% add_trace(data=data[data$Date2==unique(data$Date2)[i],], 
                             x=~hours, y=~SGvalue, mode='lines',
                             type="scatter", color=cl[i], 
                             hoverinfo = 'text',
                             text = ~paste('</br> Date: ',Date2,
                                           '</br> Time: ',time2,
                                           '</br> Sensor value :',SGvalue),
                             name = as.character(unique(data$Date2)[i]))
      }
      
      
      
    }else if (!scatterOnly & plotSummary!=""){#min, max, mean #if daily sensor
      p<-summaryLinePlot_ly(p, data, plotSummary)
      
    }
    #add bG values
    p<-addBGpoints_ly(p,data,yAxis = 'y', addBG, pointSize,startTime,endTime)
    
    #addBolusPOints
    p<-addBolusPoints_ly(p,data, addBolusType,pointSize,startTime,endTime)
    
    #add pump Settings
    p<-addPumpSetting_ly(p,addSetting, settingOverlay,startTime,endTime,startDate,endDate,
                         ay.list,xticks,yaxisStr,legendInset)
    
    
    #addPercentBG as text 
    p<-addPercentBG_ly(p,data,addPercentBG,addPercentType,fromChange = fromChange,libraryPath = libraryPath,
                       startTime = startTime, endTime = endTime)
    
    #add addBarSub of carb intake
    p<-summaryPlot_ly(p, data,
                      ay.list$ayCarb,
                     addBarSub,
                     numberDays, filterCond,
                     startDate, endDate,
                     startTime, endTime,
                     plotSummary, sumFunc = "length", stackedBar = "",
                     addBG, libraryPath = libraryPath,
                     addSetting,settingOverlay,percentSetting,
                     legendInset)
  

    #add fasting
    p<-addFasting_ly(p, data, addFasting,addFastingAnnot)
    
    #plot it
    p
    
   
    
    
    
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
  
}#end function