
rankhospital <- function(state, outcome, num = "best") {
  
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
      
      if(num=="best"){
        num=1
      }
      
      if(num=="worst"){
        num=length(data3[,11])
      }
      
      out<-data3$Hospital.Name[grep(sort(as.numeric(data3[,11]))[num],data3[,11])]
    }
    else{ 
      
      if(num=="best"){
        num=1
      }
      
      if(num=="worst"){
        num=length(data3[,11])
      }
      
      out<-data2$Hospital.Name[grep(sort(as.numeric(data2[,11])[num]),data2[,11])]
      
    }
  }
  
  
  if(outcome == "heart failure"){
    
    if(sum(is.na(as.numeric(data2[,17]))*c(1:length(data2[,17])))!=0){
      
      data3<-data2[-is.na(as.numeric(data2[,17]))*c(1:length(data2[,17])),]
      
      if(num=="best"){
        num=1
      }
      
      if(num=="worst"){
        num=length(data3[,17])
      }
      
      out<-data3$Hospital.Name[grep(sort(as.numeric(data3[,17]))[num],data3[,17])]
    }
    else{ 
      
      if(num=="best"){
        num=1
      }
      
      if(num=="worst"){
        num=length(data3[,17])
      }
      
      out<-data2$Hospital.Name[grep(sort(as.numeric(data2[,17])[num]),data2[,17])]
      
    }
  }
  
  
  
  if(outcome == "pneumonia"){
    
    if(sum(is.na(as.numeric(data2[,23]))*c(1:length(data2[,23])))!=0){
      
      data3<-data2[-is.na(as.numeric(data2[,23]))*c(1:length(data2[,23])),]
      
      if(num=="best"){
        num=1
      }
      
      if(num=="worst"){
        num=length(data3[,23])
      }
      
      out<-data3$Hospital.Name[grep(sort(as.numeric(data3[,23]))[num],data3[,23])]
    }
    else{ 
      
      if(num=="best"){
        num=1
      }
      
      if(num=="worst"){
        num=length(data3[,23])
      }
      
      out<-data2$Hospital.Name[grep(sort(as.numeric(data2[,23])[num]),data2[,23])]
      
    }
  }
  selrank=num
  toprank=min(grep(sort(as.numeric(data3[,23]))[num],sort(as.numeric(data3[,23]))))
  winner<-sort(out)[1+selrank-toprank]
  winner
}

rankhospital("TX", "heart failure", 4)
# "DETAR HOSPITAL NAVARRO"


rankhospital("MD", "heart attack", "worst")
# "HARFORD MEMORIAL HOSPITAL"


rankhospital("MN", "heart attack", 5000)
# NA

