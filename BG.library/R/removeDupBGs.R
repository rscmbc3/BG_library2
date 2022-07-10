#'@title removeDupBGs
#'@description remove duplicate BG values within 10 minute intervals
#'@param data data.frame with BG values in BGvalue
#'@return `data` data.frame with duplicates removed
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'#readBG
#'data<-read.csv(filePath, header = FALSE)
#'
#'#get pump table
#'pumpStartRow<-which(data[,1]=="Index")[1]
#'pumpEndRow<-which(data[,1]=="-------" & as.numeric(rownames(data))>pumpStartRow)-1
#'pumpData<-data[pumpStartRow:pumpEndRow,]
#'
#'#fix tables
#'pumpData<-fixtables(pumpData,libraryPath)
#'nrow(pumpData)
#'
#'#remove duplicates
#'pumpData<-removeDupBGs(pumpData)
#'nrow(pumpData)


removeDupBGs<-function(data){
  #get bg data
  bgData<-data[!is.na(data$BGvalue),]
  #remove bg data from data
  data<-data[is.na(data$BGvalue),]
 
  #flag duplicates <= 10 minutes
bgData$dup<-rep(0,nrow(bgData))
  for (b in 2:nrow(bgData)){
    if (abs(difftime(bgData$dateTime[(b-1)],bgData$dateTime[b], units = "mins"))<=10 |
        bgData$BGvalue[(b-1)]==bgData$BGvalue[b]){
      bgData$dup[b]<-1
    }
  }

#remove duplicates
bgData<-bgData[bgData$dup==0,]  
bgData<-bgData[,names(bgData)!="dup"]

#bind with non-BG data
  data<-rbind(data,bgData)
  data<-data[order(data$dateTime),]
return(data)
}
