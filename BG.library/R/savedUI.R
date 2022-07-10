#'@title savedUI
#'@description generate Shiny UI for 'saved plots'. \\cr \\cr
#'@param plotList list of saved plots within the library.  Found in
#'./BG.library/data/plotList
#'@return `ui` shiny UI for 'saved plots'.
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
#'                               selected = 'Saved Plot'),  
#'                   uiOutput("outputUI")
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
#'  
#'  output$txtOut <- renderText({
#'    paste0("These are settings for ",input$shPlotType," plots")
#'  }) 
#'})#end shiny server
#')#end shiny app

savedUI<-function(plotList){
  ui=shinyUI(
    fluidPage(
      
      selectInput("plotName","Select saved plot by name",choices = names(plotList)),
      textOutput("description")
      

      
    ))
  return(ui)
  
}