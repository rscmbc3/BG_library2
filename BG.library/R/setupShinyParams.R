#'@title setupShinyParams
#'@description Shiny server function that formats shiny inputs for use in plotting functions
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param libraryPath character string path to BG.library code
#'@param filePath character path to csv import file 
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@return `workSpace.list` list of all user inputs
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'#file path to plotList
#'plotListFile<-paste0(libraryPath,"/data/plotList")
#'#load plotList
#'load(file = plotListFile)
#'
#'shinyApp(ui=shinyUI(
#'  fluidPage(
#'    titlePanel(
#'      h1("Rshiny Interactive BG Plots")),
#'    sidebarLayout(
#'      sidebarPanel(width=6,
#'                   #top level user input
#'                   selectInput("shPlotType","Select Plot Type",
#'                               choices = c("scatter","bar","box","heatmap","Saved Plot"),
#'                               selected = 'scatter'),  
#'                   uiOutput("dateTimeUI"),
#'                   uiOutput("outputUI"),
#'                   uiOutput("historyUI")
#'      ),#end sidebar
#'      mainPanel(width = 6,
#'                verbatimTextOutput("txtOut")
#'      )#end main panel
#'    )#end sidebar layout
#'  )#end fluid page
#'),#end shiny ui
#'
#'server=shinyServer(function(input, output,session) {
#'  #render UIs
#'  observe({
#'    if (input$shPlotType=="scatter"){
#'      output$outputUI<-renderUI({
#'        scatterUI()
#'      })
#'    }else if (input$shPlotType=="box" | input$shPlotType=="bar"){
#'      output$outputUI<-renderUI({
#'        boxBarUI(data)
#'      })
#'    }else if (input$shPlotType=="heatmap"){
#'      output$outputUI<-renderUI({
#'        heatmapUI(data)
#'      })
#'    }else{#saved plot
#'      output$outputUI<-renderUI({
#'        savedUI(plotList)
#'      })
#'    }
#'  })
#'  observe({
#'    output$historyUI<-renderUI({
#'      historyUI()
#'    })
#'    output$dateTimeUI<-renderUI({
#'      dateTimeUI()
#'    })
#'    
#'    output$txtOut <- renderPrint({
#'      setupShinyParams(input, output, session,
#'                       libraryPath, filePath,
#'                       data)
#'    })  })
#'})#end shiny server
#')#end shiny app


setupShinyParams<-function(input, output, session,
                           libraryPath, filePath,
                           data){
  
  #compile all user input
  compiledInput<-compileInput(input, output, session)
  unPackList(lists = list(compiledInput = compiledInput),
             parentObj = list(NA)) 
  
  #format parameters by plot type
  if (shPlotType=="scatter"){
    timeStep<-'hour'
    period<-1
  }else if (shPlotType %in% c("bar","box")){
    ayCarb<-NA
    addBarSub<-FALSE
    boxBar<-shPlotType
    plotType<-"summaryPlot_ly"
    plotSummary<-plotSummary2
  }else if (shPlotType=="heatmap"){
    brks<-paste0("c(",brks,")")
    brks<-eval(parse(text = brks))
    tcol <- "time2"
    dcol <- "Date2"
    includeTotals<-TRUE
    naRemove<-ignoreNAs
    plotType<-"heatMap_ly"
    valueVar<-plotSummary2
    timeStep<-"hour"
    period<-1
  }
  
  #format date parameters
  startDate<-as.character(daterange[1])
  endDate<-as.character(daterange[2])
  if (exists("removeDates")){
    removeDates<-as.character(removeDates)  
  }else{
    removeDates<-NA
  }

  #format time parameters
  if (shPlotType!='heatmap'){
    startTime<-ifelse(nchar(as.character(timeRange[1]))==1,paste0("0",timeRange[1],":00"),paste0(timeRange[1],":00"))
    endTime<-ifelse(nchar(as.character(timeRange[2]))==1,paste0("0",timeRange[2],":00"),paste0(timeRange[2],":00"))
  }
  
  #set parameters not found
  if (!exists("addBarSub")){
    addBarSub<-FALSE
  }
  
  if (!exists("addSetting")){
    addSetting<-""
  }
  
  if (shPlotType=="scatter"){
    plotType<-"plotLine_ly"
  }
  
  #get all parameters needed for plotType
  if (shPlotType!="Saved Plot"){
    allArgs<-findCodeStr(libraryPath,plotType,"args")$arguments
    
    #format plot params
    paramStr<-character(0)
    for (p in 2:length(allArgs)-1){
      pName<-allArgs[p]
      pStr<-paste0(pName,"=",pName,",")
      paramStr<-paste0(paramStr,pStr)
      
    }
    
    paramStr<-paste0(paramStr,allArgs[length(allArgs)],"=",allArgs[length(allArgs)])
    
    
    
    #return workspace
    workSpace.list<-named.list(paramStr,plotType)
  }else{
    allArgs<-c("startDate","endDate","fromChange","numberDays","plotName","removeDates")
    workSpace.list<-list() 
  }
  for (n in allArgs){
    eval(parse(text = paste0("workSpace.list$",n,"<-",n)))
  }
  
  #add history seq params
  for (n in c("reportTitle","outPath","outFileName","seqType","seqLength")){
    eval(parse(text = paste0("workSpace.list$",n,"<-",n)))
  }
  
  
  return(workSpace.list)
  
}