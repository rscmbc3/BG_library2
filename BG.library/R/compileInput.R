#'@title compileInput
#'@description Shiny server function that compiles input into normal list
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@return `invalue` list of all user inputs
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'shinyApp(  ui=shinyUI(
#'fluidPage(
#'  titlePanel(
#'    h1("Rshiny Interactive BG Plots")),
#'  
#'  sidebarLayout(
#'    sidebarPanel(width=6,
#'                 #top level user input
#'                 selectInput("shPlotType","Select Plot Type",
#'                             choices = c("scatter","bar","box","heatmap","Saved Plot"),
#'                             selected = 'bar'),  
#'                 uiOutput("outputUI")
#'    ),#end sidebar
#'    
#'    mainPanel(width = 6,
#'              verbatimTextOutput("txtOut")
#'    )#end main panel
#'  )#end sidebar layout
#')#end fluid page
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
#'        savedUI()
#'      })
#'    }
#'  })
#'  
#'  output$txtOut <- renderPrint({
#'    compileInput(input, output, session)
#'  }) 
#'  
#'})#end shiny server
#')#end shiny app


compileInput<-function(input, output, session){
  
  invalue<-list()
  for (n in names(input)){
      eval(parse(text = paste0("invalue$`",n,"`<-input$`",n,"`")))
    }

  return(invalue)
} 
