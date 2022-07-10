#'@title summaryPlot_ly
#'@description generate bar and box summary plots and carb subbarplot  \\cr \\cr
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param p current plot_ly plot
#'@param barSubPlot TRUE/FALSE whether subplot should be added to main plot
#'@param ayCarb list of y axis specifications for carb barplot
#'@return `p` plot_ly interactive plot


summaryPlot_ly<-function(p, data,
                         ayCarb,
                     addBarSub = FALSE,
                     boxBar = "bar",
                     numberDays, filterCond = "",
                     startDate = NA, endDate = NA,removeDates = NA,
                     startTime = "00:00", endTime = "23:00",
                     timeStep = "hour",period = 1,fromChange = TRUE,libraryPath,
                     plotSummary, sumFunc = "length", stackedBar = "",
                     uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = FALSE,
                     addGoodRange = FALSE,addBG = TRUE, pointSize = 10,
                     addSetting = "",settingOverlay = FALSE,percentSetting = 30,
                     legendInset = -0.2,description = "",descInset = -0.15){


  
  if (addBarSub){#barplot is not main plot and add subplot
    
    #subset settings
    pumpSettings.list<-subsetSetting(data,libraryPath)
    unPackList(lists = list(pumpSettings.list = pumpSettings.list),
               parentObj = list(NA)) 
    
    p <- barSubPlot_ly(p, data, addBarSub,timeStep, period)
    return(p)
    
  }else if (timeStep=="hour"){#barplot is main plot timestep hour
    p <- summaryPlotHour_ly(data,addBarSub,boxBar,
                        numberDays, filterCond,
                        startDate, endDate,removeDates,
                        startTime, endTime,
                        timeStep,period,fromChange,libraryPath,
                        plotSummary, sumFunc, stackedBar,
                        uniqueDT,replaceNAs,ignoreNAs,
                        addGoodRange,addBG, pointSize,
                        addSetting,settingOverlay,percentSetting,
                        legendInset,description,descInset)
      p
  }else if (timeStep=="day"){#barplot is main plot timestep day
    p <- summaryPlotDay_ly(data,addBarSub,boxBar,
                            numberDays, filterCond,
                            startDate, endDate,removeDates,
                            startTime, endTime,
                            timeStep,period,fromChange,libraryPath,
                            plotSummary, sumFunc, stackedBar,
                            uniqueDT,replaceNAs,ignoreNAs,addGoodRange,
                            legendInset,description,descInset)
      p
  }else{
      return(p)
    }
}#end function