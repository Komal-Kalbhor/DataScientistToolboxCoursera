setwd("D:\\BI\\Coursera\\Courses\\2R\\week4\\rprog-data-ProgAssignment3-data")

data1<-read.csv("outcome-of-care-measures.csv",na.strings = c("NA","Not Available"))

rankhospital<-function(state,outcome,num="best")
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
  
  if(num == "best") {
    num = 1
  }
  else if(num == "worst") {
    num = nrow(data1.state)
  }
  else if(is.numeric(x=num)) {
    if(num<1 || num > nrow(data1.state)) {
      return(NA)}
  }
  else {
    stop('invalid num')
  }
  
  data1.state <- data1.state[order(data1.state[,i], data1.state$Hospital.Name),]
  hnames <- data1.state[num, ]$Hospital.Name
  hnames
}

rankhospital("TX","heart failure",4)
rankhospital("MN", "heart attack", 5000)
rankhospital("MD", "heart attack", "worst")

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
