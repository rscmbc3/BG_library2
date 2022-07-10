#'@title boxBarUI
#'@description generate Shiny UI for 'box' and 'bar plots. \\cr \\cr
#'@param data data.frame to use for shiny plots
#'@return `ui` shiny UI for 'box' and 'bar' plots.
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'#load functions
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
#'                               selected = 'heatmap'),  
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


heatmapUI<-function(data){
  ui=shinyUI(
    fluidPage(
      

      
      h3("Add data to plot"),
      selectInput("plotSummary2","Data to summarize",
                  choices = c("",names(data)),
                  selected = "Sensor.Glucose..mg.dL."),
      textInput("sumFunc","Summary Function to be applied per time step",value = "mean"),
      
      textInput("brks","Set breakpoints for heatmap colors (must not exceed number of colors in pallete)",
                value = "0,50,80,150,240,300,400,500"),
      checkboxInput("addHist","Add histogram of color groups in heatmap", value = TRUE),
      
      checkboxInput("replaceNAs","Replace missing values with zeros",value = FALSE),
      checkboxInput("ignoreNAs","Remove rows with missing data",value = TRUE),
      
      h3("General Plotting Parameters"),
      selectInput("brewerPallete","Select RColorBrewer pallete for heatmap (pallete must have numberColors>= number of breakpoints",
                  choices = rownames(RColorBrewer::brewer.pal.info),
                  selected = "RdBu"),
      checkboxInput("revPallete","Reverse color pallete",value = TRUE),
      textInput("textCol","Color of text in heatmap cells",value = "black"),
      
      textInput("filterCond","Filter data according to R syntax statement 
          using 'data' as the data.frame name and column names from input file",value = ""),
      
      numericInput("legendInset","Value to place legend below plot (must be negative)",max = 0, value = -0.2),
      textInput("description","Optional plot description to output below plot area",value = ""),
      numericInput("descInset","Value to place plot description below plot area (must be negative)",max = 0, value = -0.15)
      
      
    ))
  return(ui)
  
}