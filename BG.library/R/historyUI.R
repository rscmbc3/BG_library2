#'@title historyUI
#'@description generate Shiny UI for historical sequence plots. \\cr \\cr
#'@param data data.frame to use for shiny plots
#'@return `ui` shiny UI for historical sequence plots.
#'@examples
#'shinyApp(  ui=shinyUI(
#'  fluidPage(
#'    titlePanel(
#'      h1("Rshiny Interactive BG Plots")),
#'    
#'    sidebarLayout(
#'      sidebarPanel(width=6,
#'                   uiOutput("historyUI")
#'      ),#end sidebar
#'      
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
#'    output$historyUI<-renderUI({
#'      historyUI()
#'    })
#'  })
#'  
#'  output$txtOut <- renderPrint({
#'    paste0("These are settings for historical sequence plots")
#'  }) 
#'  
#'})#end shiny server
#')#end shiny app

historyUI<-function(){
  ui=shinyUI(
    fluidPage(
      
      #historytSeq
      h3("Historical Sequence Plot Parameters"),
      checkboxInput("outPutHistorySeq","Output a sequence of historical plots as html",value = TRUE),
      conditionalPanel(
        condition = "input.outPutHistorySeq==1",
        textInput("reportTitle","Title for output html document"),
        textInput("outPath","Path to directory in which to output html document"),
        h6("Blank will output file to ~BG.library/reports"),
        textInput("outFileName","Name of output file"),
        selectInput("seqType","Type of historical sequence to output",choices = c("change","days"),
                    selected = "change"),
        numericInput("seqLength", "Number of repetitions of historical sequence plots", value = 2),
        numericInput("periodHist","Number of Days for each historical sequence plot",value = 7)
      )
      
    ))
  return(ui)
  
}