summaryPlotHour_ly<-function(data,addBarSub,boxBar,
                         numberDays, filterCond = "",
                         startDate = NA, endDate = NA,removeDates = NA,
                         startTime = "00:00", endTime = "23:00",
                         timeStep = "hour",period = 1,fromChange,libraryPath,
                         plotSummary, sumFunc = "length", stackedBar = "",
                         uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = FALSE,
                         addGoodRange = FALSE,addBG = TRUE, pointSize = 10,
                         addSetting = "",settingOverlay = FALSE,percentSetting = 30,
                         legendInset = -0.2,description,descInset){
  
  #subset data by date and filterCond
  data<-subsetData(data,numberDays = numberDays,startDate = startDate,
                   endDate = endDate,filterCond = filterCond,
                   startTime = startTime, endTime = endTime,
                   timeStep = timeStep,period = period, fromChange = fromChange, 
                   removeDates = removeDates,libraryPath =libraryPath)

  #data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
  
  #subset settings
  pumpSettings.list<-subsetSetting(data,libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  #if filtered data exists
  if(nrow(data)!=0){ 
    #get hours as number
    data$hours<- data$hour + data$min/60
    
    #save data for addPumpSetting_ly and addBGpoints_ly
    #basalOrig<-basal
    dataOrig<-data


    if (stackedBar!="insulin" | boxBar!="bar"){
      data$temp<-eval(parse(text = paste0("data$",plotSummary)))
      
      #ignoreNAs
      if (ignoreNAs){
        data<-data[!is.na(data$temp),]
      }
      
      #get uniques
      if (uniqueDT){
        NAMES<-c("dateTime","Date2","hours","hour","temp")
        data<-uniqueDateTime(data, NAMES, replaceNAs,startTime = startTime,endTime = endTime,timeStep = timeStep, period = period)

        }
      
      data<-data[c("hour","temp")]
      
      
      
      #set initial y axis range
      if(plotSummary %in% c("BGvalue","SGvalue") & sumFunc!="length"){
        initYrange<-c(0,450)
        
      }else if (sumFunc=="length"){
        rangeData<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(length)))
        initYrange<-c(0,max(rangeData$temp, na.rm = TRUE))
      }else{
        initYrange<-c(0,max(data$temp, na.rm = TRUE))
      }
      
      yTitle<-ifelse(sumFunc!="length",paste0(sumFunc,"_",plotSummary),paste0("number_",plotSummary))
      dataFormat<-data
      
    }else{#stacked insulin
      basal$rate<-basal[[length(basal)]]
      
      #format time
      basal$time2<-as.POSIXlt(basal$time,format="%H:%M")
      basal$hours<- basal$time2$hour + basal$time2$min/60 
      basal$hour<-basal$time2$hour
      basal2<-setTimeStep(basal, startTime,endTime, timeStep, period)
      
      #get on hour values only
      basal2<-basal2[,c("hours","hour","rate")]
      
      basal2<-as.data.frame(basal2 %>% group_by(hour) %>% summarise_all(funs(mean),na.rm=TRUE))
      
      data$hours<- data$hour + data$min/60 
      
      #only look at pump data
      data<-data[is.na(data$SGvalue),]
      
      #get unique values (max) per Date2 and hours
      NAMES<-c("dateTime","Date2","hours","hour","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
      data<-uniqueDateTime(data, NAMES, replaceNAs = TRUE,startTime = startTime,endTime = endTime,timeStep = timeStep, period = period,sumFunc = "max")
      
      #summarize by mean
      data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(mean),na.rm=TRUE))
      
      
      #merge with basal
      data<-merge(data, basal2, by = "hour", all = TRUE)
      data<-data[c("hour","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.","rate")]
      names(data)[1]<-"hour"
      
      
      

      orderVector<-c("hour","rate","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
      data<-data[,match(orderVector, names(data))]
      
      names(data)[2]<-"basal rate"
      
      #set initial y axis range
      data$totalInsulin<-apply(data[-c(1)],1, function(x) sum(x,na.rm = TRUE))
      initYrange<-c(0,max(data$totalInsulin, na.rm = TRUE))
      data<-data[-c(length(data))]
      yTitle<-"Insulin Units Delivered"
      
      dataFormat<-data
     
    }
    

  #format time in decimal hours for xaxis tick marks
  xticks.list<-xTicks(data = dataOrig, startTime,endTime,timeStep,period)
  unPackList(lists = list(xticks.list = xticks.list),
             parentObj = list(NA)) 

    
    #get yaxis code string
    yaxisStr.list<-makeYaxesSummary(addSetting, settingOverlay, percentSetting,addBarSub,addBG,
                                initYrange,yTitle)
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
    layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange = addGoodRange,stackedBar = stackedBar,
                          description = description,descInset = descInset)
    
    #initialize plot
    p<-plot_ly()
    #add layout
    eval(parse(text = layoutStr))
    #get formatted data
    data<-dataFormat
    
    if(stackedBar=="" | boxBar!="bar") {#general bar plot
       
      if (boxBar=="bar"){
      #set up summary function string
      if (sumFunc!="length"){
        sumString<-paste0("as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(",
                          sumFunc,"),na.rm = TRUE))")
      }else if (sumFunc=="length"){
        sumString<-paste0("as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(",
                          sumFunc,")))")
        
      }#end sumString
      
      #apply sumString
      data<-eval(parse(text = sumString))
      
      #create plot
             p <- p %>% add_trace(data = data, x = ~hour, y = ~temp,type = 'bar', 
                           name = paste0(sumFunc,"_",plotSummary)) 
      }else{#boxplot
        
        p <- p %>% add_trace(data = data, x = ~hour, y = ~temp,type = 'box', 
                             name = paste0(sumFunc,"_",plotSummary)) 
      }

      
      
      
    }else if (stackedBar=="insulin"){
      cls<-c("white","gray","black")
      p <- addStackbar_ly(p,data,cls,timeStep) 
      
    }else{#stacked bar BG
      data$veryHigh<-ifelse(data$temp>240,1,0)
      data$high<-ifelse(data$temp>150 & data$temp<=240,1,0)
      data$good<-ifelse(data$temp>=80 & data$temp<=150,1,0)
      data$low<-ifelse(data$temp<80,1,0)
      data<-data[,names(data)!="temp"]
      data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(sum),na.rm=TRUE))
      orderVector<-c("hour","low","good","high","veryHigh")
      data<-data[,match(names(data),orderVector)]
      cls<-c("blue","white","red","darkred")
      p <- addStackbar_ly(p,data,cls,timeStep) 
      
    }
    #add pump Settings
    p<-addPumpSetting_ly(p,addSetting, settingOverlay,startTime,endTime,startDate,endDate,
                         ay.list,xticks,yaxisStr,legendInset)
    
    #add bG values
    p<-addBGpoints_ly(p,data = dataOrig, yAxis = 'y7', addBG, pointSize)
    return(p)
    
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}