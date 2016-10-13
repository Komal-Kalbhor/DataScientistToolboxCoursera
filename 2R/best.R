setwd("D:\\BI\\Coursera\\Courses\\2R\\week4\\rprog-data-ProgAssignment3-data")

data1<-read.csv("outcome-of-care-measures.csv",na.strings = c("NA","Not Available"))

best<-function(state,outcome)
{
  if(!any(state == data1$State)) {
    stop('invalid state')
  }
  
  if(outcome=="heart attack")
    i <- 11
  else if(outcome=="heart failure")
    i <- 17
  else if(outcome=="pneumonia")
    i <- 23
  else
    stop('invalid outcome')
  
  data1.state <- data1[data1$State == state, ]
  
  data1.state <- data1.state[complete.cases(data1.state[,i]),]
  x <- data1.state[(data1.state[,i] == min(data1.state[,i])),]
  sort(x)[1]
  x[2]
}

best("TX","heart attack")
best("TX","heart failure")
best("MD", "heart attack")
best("MD", "pneumoniaa")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
