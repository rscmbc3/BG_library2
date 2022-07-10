#'@title replaceTitle
#'@description Replace title of RMD reports with custom title  \\cr \\cr
#'@param libraryPath character string path to BG.library code 
#'@param reportTitle character string title to use for RMD document
#'@param reportName character string indicating the name of the report to 
#'edit (i.e. "BG_report","historySeqOut")
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'reportTitle<-"This is my new BG_report"
#'replaceTitle(libraryPath, reportTitle,reportName = "BG_report")

replaceTitle<-function(libraryPath, reportTitle, reportName){
  
  RMDpath<-paste0(libraryPath,"reports/",reportName,".Rmd")
  #read Rmd file as text
  x <- readLines(RMDpath)
  #find where title is designated
  editthis<-x[which(regexpr("title:",gsub(" ","",x))>0)]
  #replace with current reportTitle
  y <- gsub( editthis, paste0("title: '",reportTitle,"'"), x )
  #overwrite the file
  cat(y, file=RMDpath, sep="\n") 
}
