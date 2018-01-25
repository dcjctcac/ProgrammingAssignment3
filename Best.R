
best <- function(state, outcome) {
  
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(sum(state==data1$State)==0){
    stop(print("invalid state"))
  }

  data2<-data1[-c(state!=data1$State)*c(1:length(data1$State)),]
  
  if(sum(outcome==c("heart attack","heart failure","pneumonia"))!=1){
    stop(print("invalid outcome"))
  }
  
  if(outcome == "heart attack"){
    
    if(sum(is.na(as.numeric(data2[,11]))*c(1:length(data2[,11])))!=0){
      
      data3<-data2[-is.na(as.numeric(data2[,11]))*c(1:length(data2[,11])),]
      
      out<-data3$Hospital.Name[grep(min(as.numeric(data3[,11])),data3[,11])]
    }
    else{ out<-data2$Hospital.Name[grep(min(as.numeric(data2[,11])),data2[,11])]}
  }
  

  if(outcome == "heart failure"){
    
    if(sum(is.na(as.numeric(data2[,17]))*c(1:length(data2[,17])))!=0){
      
      data3<-data2[-is.na(as.numeric(data2[,17]))*c(1:length(data2[,17])),]
      
      out<-data3$Hospital.Name[grep(min(as.numeric(data3[,17])),data3[,17])]
    }
    else{ out<-data2$Hospital.Name[grep(min(as.numeric(data2[,17])),data2[,17])]}
  }
  
  
  if(outcome == "pneumonia"){
    
    if(sum(is.na(as.numeric(data2[,23]))*c(1:length(data2[,23])))!=0){
      
    data3<-data2[-is.na(as.numeric(data2[,23]))*c(1:length(data2[,23])),]
    
    out<-data3$Hospital.Name[grep(min(as.numeric(data3[,23])),data3[,23])]
    }
    else{ out<-data2$Hospital.Name[grep(min(as.numeric(data2[,23])),data2[,23])]}
  }
  winner<-sort(out)[1]
  winner
}




best("TX", "heart attack")
# "CYPRESS FAIRBANKS MEDICAL CENTER"

best("TX", "heart failure")
#"FORT DUNCAN MEDICAL CENTER"

best("MD", "heart attack")
#"JOHNS HOPKINS HOSPITAL, THE"

best("MD", "pneumonia")
#"GREATER BALTIMORE MEDICAL CENTER"

best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state

best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome