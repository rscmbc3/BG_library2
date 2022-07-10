#setPaths
libraryPath<-"G:/BG_library_tandem/BG.library/"
BGfilePath<-"G:/Lily Sanisaca 07-10-2022.csv"
SGfilePath<-"G:/Clarity_Export_Gorman_Sanisaca_Lillian_2022-07-10_144607.csv"

#load functions
devtools::load_all(libraryPath,recompile = FALSE) 

#get pumpSettings
pumpSettings.list<-makePumpSettings(libraryPath)
unPackList(lists = list(pumpSettings.list = pumpSettings.list),
           parentObj = list(NA)) 

#csv data import
dataImport.list<-dataImport(BGfilePath,SGfilePath,libraryPath)
unPackList(lists = list(dataImport.list = dataImport.list),
           parentObj = list(NA)) 
#BG report
generateBGreport(libraryPath, BGfilePath, SGfilePath, data = allData)
generateBGreport(libraryPath,BGfilePath, SGfilePath, data = allData, removeDates = c("2019-10-31","2019-11-01"))
generateBGreport(libraryPath, BGfilePath, SGfilePath, data = allData,numberDays = NA,
                 fromChange = FALSE,startDate = "2022-07-05", endDate=NA)


#shiny app
runApp(shinyPlot(libraryPath, BGfilePath, SGfilePath), launch.browser = TRUE)

#summarize data
BGvalue_Summary<-summarizeData(allData, colName = "BG.Reading..mg.dL.", libraryPath = libraryPath,
                               removeDates = "2019-10-28")
BGvalue_SummaryDaily<-summarizeData(allData, colName = "BG.Reading..mg.dL.",  timeStep = "day", libraryPath = libraryPath)
BGpercent_Summary<-addPercentBG_ly(data = allData, p = NA,addPercentBG = c("very high","high","good","low"),
                                   addPercentType = "BG.Reading..mg.dL.",outputType = "table",
                                   libraryPath = libraryPath, removeDates = "2019-10-28")
SGpercent_Summary<-addPercentBG_ly(data = allData, p = NA,addPercentBG = c("very high","high","good","low"),
                                   addPercentType = "Sensor.Glucose..mg.dL.",outputType = "table",
                                   libraryPath = libraryPath)
BGHigh_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", 
                            sumFuncs = "length",
                            filterCond = "data[data$BG.Reading..mg.dL.>150 & !is.na(data$BG.Reading..mg.dL.),]", libraryPath = libraryPath)
BGveryHigh_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", 
                                sumFuncs = "length",
                                filterCond = "data[data$BG.Reading..mg.dL.>240 & !is.na(data$BG.Reading..mg.dL.),]", libraryPath = libraryPath)

BGLow_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", 
                           sumFuncs = "length",
                           filterCond = "data[data$BG.Reading..mg.dL.<80 & !is.na(data$BG.Reading..mg.dL.),]", libraryPath = libraryPath)
BGgood_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", 
                            sumFuncs = "length",
                            filterCond = "data[data$BG.Reading..mg.dL.>=80 & data$BG.Reading..mg.dL.<=150 & !is.na(data$BG.Reading..mg.dL.),]",
                            libraryPath = libraryPath)
tempBasal_count<-summarizeData(allData, colName = "Temp.Basal.Amount", 
                               sumFuncs = "length",
                               filterCond = "data[data$Temp.Basal.Amount==0 & !is.na(data$Temp.Basal.Amount),]",
                               libraryPath = libraryPath)
suspendBasal_Count<-summarizeData(allData, colName = "Alarm", 
                                  sumFuncs = "length",
                                  filterCond = "data[regexpr('SUSPEND',data$Alarm)>0 & !is.na(data$Alarm),]", 
                                  libraryPath = libraryPath)


#timeDayTables used for heatmaps
BGvalue_timeDaytable<-timeDayTable(allData, tcol = "time2", dcol = "Date2", 
                                   valueVar = "BG.Reading..mg.dL.", 
                                   sumFunc = "mean", naRemove = TRUE,
                                   includeTotals = TRUE,
                                  filterCond = "",
                                   libraryPath = libraryPath,removeDates = "2019-10-28")
SGvalue_timeDaytable<-timeDayTable(allData, tcol = "time2", dcol = "Date2", 
                                   valueVar = "Sensor.Glucose..mg.dL.", 
                                   sumFunc = "mean", naRemove = TRUE,
                                   includeTotals = TRUE,
                                   filterCond = "",
                                   libraryPath = libraryPath)
carbs_timeDaytable<-timeDayTable(allData, tcol = "time2", dcol = "Date2", 
                                 valueVar = "BWZ.Carb.Input..grams.", 
                                 sumFunc = "max", naRemove = TRUE,
                                 includeTotals = TRUE,replaceNAs = TRUE,
                                 filterCond = "",
                                 libraryPath = libraryPath)

#saved plots
#linePlots
executeSavedPlot(data = allData, plotName = "lineSumSens_SGper_Sett_BG", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "lineSumSens_BGper_Sett_BG", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "lineSumSens_BGper_subCarb_BG", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "lineSumSens_BGper_subCarb_Sett_BG", libraryPath = libraryPath)
#barplots hourly
executeSavedPlot(data = allData, plotName = "sumBar_highBG150_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "sumBar_lowBG80_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "stackBarInsulinHour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "stackBarBGallHour_Sett", libraryPath = libraryPath)
#every 3 hours barplots
executeSavedPlot(data = allData, plotName = "stackBarBG3Hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "stackBarInsulin3Hour_Sett", libraryPath = libraryPath)
#daily barplots
executeSavedPlot(data = allData, plotName = "stackBarInsulinDaily", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "stackBarBGDaily", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "stackBarSGDaily", libraryPath = libraryPath)
########boxplots hourly
executeSavedPlot(data = allData, plotName = "boxSGhour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "boxBGhour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "boxCorrUhour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "boxFoodUhour_Sett", libraryPath = libraryPath)
#3hour boxplots
executeSavedPlot(data = allData, plotName = "boxSG3hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "boxBG3hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "boxCorrU3hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "boxFoodU3hour_Sett", libraryPath = libraryPath)
##daily boxplots
executeSavedPlot(data = allData, plotName = "boxSGdaily", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "boxBGdaily", libraryPath = libraryPath)
#heatmaps
executeSavedPlot(data = allData, plotName = "meanBGheat_hist", libraryPath = libraryPath)
executeSavedPlot(data = allData, plotName = "meanSGheat_hist", libraryPath = libraryPath)

#dateSeq Reports
#saved plot
historySeqOut(data = NA,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare Summary Sensor Line Plot Since Last Pump Setting Change" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
                        seqType = "change", seqLength = 2, removeDates = c("2019-10-31","2019-11-01"))
historySeqOut(data = NA,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare SG box Plot Since Last Pump Setting Change" ,
              plotName = "boxSGhour_Sett", paramList = NA, plotType = NA,
              seqType = "change", seqLength = 2)
historySeqOut(data = NA,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare BG daily box Plot Since Last Pump Setting Change" ,
              plotName = "boxBGdaily", paramList = NA, plotType = NA,
              seqType = "change", seqLength = 2)
historySeqOut(data = NA,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare Heat Maps of mean BG values Since Last Pump Setting Change" ,
              plotName = "meanBGheat_hist", paramList = NA, plotType = NA,
              seqType = "change", seqLength = 2) 
historySeqOut(data = NA,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare Heat Maps of mean BG values Since Last Pump Setting Change" ,
              plotName = "meanSGheat_hist", paramList = NA, plotType = NA,
              seqType = "change", seqLength = 2) 
historySeqOut(data = NA,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare Summary Sensor Line Plot Since Last Pump Setting Change" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
              seqType = "change", seqLength = 2, outPath ="F:/",outFileName = "testOut") 
historySeqOut(data = NA,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare All Pump Setting Changes Summary Sensor Line Plot" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
              seqType = "change", seqLength = "all")
historySeqOut(data = allData,libraryPath = libraryPath, filePath = filePath,reportTitle = "Compare Summary Sensor Line Plot Weekly" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
              seqType = "days", seqLength = 3, period = 7) 
#new plot parameters
parmList1<-list(scatterOnly = FALSE, pointSize = 10,
               startTime = "00:00", endTime = "23:00",
               addSensor = FALSE, addBG = TRUE,
               addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U."),
               addBarSub = FALSE,
               plotSummary = "Sensor.Glucose..mg.dL.",
               libraryPath = libraryPath)
historySeqOut(data = NA,libraryPath, filePath,reportTitle = "Bolus Type Points Summary Sensor Line Plot" ,
              plotName = NA, paramList = parmList1, plotType = "plotLine_ly",
              seqType = "change", seqLength = 2) 
#history sequence in Rstudio
historySeq(data = allData,plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
           seqType = "change", seqLength = 2, libraryPath = libraryPath,removeDates = "2019-10-28")
historySeq(data = allData,plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
           seqType = "day", seqLength = 2,period = 7, libraryPath = libraryPath)





#line plot
plotLine_ly(allData,  scatterOnly = FALSE, pointSize = 10,
                      numberDays = 5, startDate = "2019-09-08", endDate = "2019-09-08",
                      startTime = "00:00", endTime = "23:00",
                      colorPalleteDaily = "rainbow", 
                      addBG = TRUE, settingOverlay = FALSE,
            addBolusType = "Bolus.Volume.Delivered..U.",
            #addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U."),
            #addBolusType = "",   
            addBarSub = FALSE,libraryPath = libraryPath,
            plotSummary = "Sensor.Glucose..mg.dL.",
                      addSetting ="",filterCond = "",
                      legendInset = -0.2, removeDates = "2019-10-28")
#barplots
summaryPlot_ly(p = NA, data = allData, ayCarb = NA,
                        addBarSub = FALSE,boxBar = "bar",
                        numberDays = 5, filterCond = "",
                        startDate = NA, endDate = NA,
                        startTime = "06:00", endTime = "18:00",
                        plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
                        addBG = FALSE, libraryPath = libraryPath,
                        addSetting = "",settingOverlay = FALSE,percentSetting = 30,
                        legendInset = -0.2, removeDates = "2019-10-28")


#simple with setting subplot
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
              addBarSub = FALSE,
              numberDays = 5, filterCond = "",
              startDate = NA, endDate = NA,
              startTime = "00:00", endTime = "23:00",
              plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
              uniqueDT = TRUE,replaceNAs = FALSE,
              addBG = TRUE, 
              addSetting = c("basal","corrFactor","carbRatio"),settingOverlay = FALSE,percentSetting = 30,
              legendInset = -0.2, libraryPath = libraryPath)

#simple barplot period 3 hours
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
              addBarSub = FALSE,
              numberDays = 5, filterCond = "",
              startDate = NA, endDate = NA,
              startTime = "00:00", endTime = "23:00",period = 3,
              plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
              uniqueDT = TRUE,replaceNAs = FALSE,ignoreNAs = TRUE,
              addBG = FALSE, 
              addSetting = c("basal","corrFactor","carbRatio"),settingOverlay = FALSE,percentSetting = 30,
              legendInset = -0.2)
#simple barplot timeStep day
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
           addBarSub = FALSE,
           numberDays = 5, filterCond = "",
           startDate = NA, endDate = NA,
           startTime = "00:00", endTime = "23:00",period = 1,timeStep = "day",
           plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
           uniqueDT = TRUE,replaceNAs = FALSE,ignoreNAs = TRUE,
           addBG = FALSE, 
           addSetting = "",settingOverlay = FALSE,percentSetting = 30,
           legendInset = -0.2)
#boxplots
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
               addBarSub = FALSE,boxBar = "box",
               numberDays = 5, filterCond = "",
               startDate = NA, endDate = NA,
               startTime = "00:00", endTime = "23:00",
               plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "", stackedBar = "",
               addBG = FALSE, uniqueDT = TRUE,replaceNAs = FALSE,ignoreNAs = TRUE,
               addSetting = c("basal","corrFactor","carbRatio"),settingOverlay = FALSE,percentSetting = 30,
               legendInset = -0.2)
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
               addBarSub = FALSE,plotType = "box",
               numberDays = 5, filterCond = "",
               startDate = NA, endDate = NA,
               startTime = "00:00", endTime = "23:00",timeStep = "day",
               plotSummary ="BWZ.Correction.Estimate..U.", sumFunc = "", stackedBar = "",
               addBG = FALSE, uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = TRUE,
               addSetting = "",settingOverlay = FALSE,percentSetting = 30,
               legendInset = -0.2)
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
               addBarSub = FALSE,plotType = "box",
               numberDays = 5, filterCond = "",
               startDate = NA, endDate = NA,
               startTime = "00:00", endTime = "23:00",timeStep = "day",
               plotSummary ="BWZ.Food.Estimate..U.", sumFunc = "", stackedBar = "",
               addBG = FALSE, uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = TRUE,
               addSetting = "",settingOverlay = FALSE,percentSetting = 30,
               legendInset = -0.2)


#plotly heat maps
heatMap_ly(brks = c(0,50,80,150,240,300,400,500), 
           brewerPallete = "RdBu", revPallete = TRUE,
           textCol = "black",
           #timeDayTable args
           data = allData, tcol = "time2", dcol = "Date2", 
           valueVar = "BG.Reading..mg.dL.", 
           sumFunc = "mean", naRemove = TRUE,
           filterCond = "",
           libraryPath = libraryPath,
           startDate = "2019-09-23",endDate = "2019-10-01", fromChange = FALSE, removeDates = "2019-09-25")
heatMap_ly(brks = c(0,50,80,150,240,300,400,500), 
           brewerPallete = "RdBu", revPallete = TRUE,
           textCol = "black",
           #timeDayTable args
           data = allData, tcol = "time2", dcol = "Date2", 
           valueVar = "Sensor.Glucose..mg.dL.", 
           sumFunc = "mean", naRemove = TRUE,
           includeTotals = TRUE,
           filterCond = "",
           libraryPath = libraryPath)


