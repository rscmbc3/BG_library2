#'@title shinyReport
#'@description Shiny server function that outputs BG_report and historical sequence 
#'plto reports as html documents
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param libraryPath character string path to BG.library code
#'@param filePath character path to csv import file 
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param reportType character string indicating the type of report to be generated
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

shinyReport<-function(input, output, session,
                          libraryPath, BGfilePath, SGfilePath,
                          data, reportType){
  shinySetup.list<-setupShinyParams(input, output, session,
                                    libraryPath, filePath,
                                    data)
  unPackList(lists = list(shinySetup.list = shinySetup.list),
             parentObj = list(NA)) 
  if (input$shPlotType!="Saved Plot"){
    
    
    #build parmList
    execStr<-paste0("parmList<-list(",paramStr,")")
    eval(parse(text = execStr))
    
    #render RMD
    plotName<-NA
    paramList<-parmList
    
    paramList<-paramList[!names(paramList) %in% c("data","numberDays")]
  }else{
    
    plotName<- input$plotName
    paramList<-NA
    plotType<-NA
    
  }
  
  if (reportType=="history"){
    period<-input$periodHist
  historySeqOut(data = NA,plotName, paramList = paramList, plotType,
                seqType, seqLength, period = period,
                reportTitle,outPath, outFileName,removeDates = removeDates,
                libraryPath,BGfilePath=BGfilePath, SGfilePath=SGfilePath)
  
  }else{#BG report
    generateBGreport(libraryPath, BGfilePath, SGfilePath, data = data,numberDays = numberDays,
                     fromChange = fromChange,startDate = startDate, 
                     endDate=endDate,removeDates = removeDates)
    }
  
  
  
}