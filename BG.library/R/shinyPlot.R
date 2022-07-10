#'@title shinyPlot
#'@description Shiny application for generating interactive plotly plots, BG_report, and 
#'historical sequence plots with user-friendly shiny user inputs
#'@param libraryPath character string path to BG.library code
#'@param filePath character path to csv import file 
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'shinyPlot(libraryPath, filePath)


shinyPlot<-function(libraryPath, BGfilePath, SGfilePath){
  #load functions
  devtools::load_all(libraryPath,recompile = FALSE) 
  
  #get pumpSettings
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  #csv data import
  dataImport.list<-dataImport(BGfilePath,SGfilePath,libraryPath)
  unPackList(lists = list(dataImport.list = dataImport.list),
             parentObj = list(NA)) 
  
  data<-allData
  
  #file path to plotList
  plotListFile<-paste0(libraryPath,"/data/plotList")
  
  #load plotList
  load(file = plotListFile)
  
  #start shiny app
  shinyApp(  ui=shinyUI(
    
    fluidPage(tags$head(
      tags$style("h5{color: red}")),
      titlePanel(
        h1("Rshiny Interactive BG Plots")),
      
      sidebarLayout(
        sidebarPanel(width=4,
                     #plot actionButtons
                     fluidRow(actionButton("goPlot","Generate Plot"),
                              actionButton("goHistory","Generate History Report"),
                              actionButton("goReport","Generate BG Report")),
                     
                     h4("Plot Specifications                     "),
                     br(),
                     #top level user input
                     selectInput("shPlotType","Select Plot Type",
                                 choices = c("scatter","bar","box","heatmap","Saved Plot")),  
                     
                     uiOutput("dateTimeUI"),
                     uiOutput("outputUI"),
                     uiOutput("historyUI")
                     
                     
        ),#end sidebar
        
        mainPanel(width = 8,
                  plotlyOutput("plotOne", width=900,height=900)
        )#end main panel
      )#end sidebar layout
    )#end fluid page
  ),#end shiny ui
  
  #shiny server function
  server=shinyServer(function(input, output,session) {
    #render UIs
    observe({
      if (input$shPlotType=="scatter"){
        output$outputUI<-renderUI({
          scatterUI()
        })
      }else if (input$shPlotType=="box" | input$shPlotType=="bar"){
        output$outputUI<-renderUI({
          boxBarUI(data)
        })
      }else if (input$shPlotType=="heatmap"){
        output$outputUI<-renderUI({
          heatmapUI(data)
        })
      }else{#saved plot
        output$outputUI<-renderUI({
          savedUI(plotList)
        })
      }
      
    })
    
    #set ui's common to all plots
    observe({
      output$historyUI<-renderUI({
        historyUI()
      })
      output$dateTimeUI<-renderUI({
        dateTimeUI()
      })
    })
    
    observe({
      #saved plot description  
      output$description <- renderText({
        eval(parse(text = paste0("plotList$",input$plotName,"$description")))
      })  
    })
    
    
    
    
    #plot
    p<-eventReactive(input$goPlot,{
      goShinyPlot(input, output, session,
                  libraryPath, filePath,
                  data)
    })   
    #interactive plot
    observe({
      output$plotOne  <- renderPlotly({
        p()
        
      })#end renderplot
    }) 
    
    #generate reports
    hs<-eventReactive(input$goHistory,{
      shinyReport(input, output, session,
                  libraryPath = libraryPath,       
                  BGfilePath = BGfilePath,
                  SGfilePath = SGfilePath,
                  data = data, reportType = "history")
    })      
    
    bg<-eventReactive(input$goReport,{
      shinyReport(input, output, session,
                  libraryPath = libraryPath,       
                  BGfilePath = BGfilePath,
                  SGfilePath = SGfilePath,
                  data = data,reportType = "BG")
    })
    
    observe({ 

      hs()
    })
    observe({ 
      bg()
      
    })
    
    
  })#end shiny server
  )#end shiny app
}#end function