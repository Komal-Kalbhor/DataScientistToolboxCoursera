setwd("D:\\BI\\Coursera\\Courses\\2R\\week4\\rprog-data-ProgAssignment3-data")

data1<-read.csv("outcome-of-care-measures.csv",na.strings = c("NA","Not Available"))

rankall<-function(outcome,num)
{
  if(outcome=="heart attack")
    i <- 11
  else if(outcome=="heart failure")
    i <- 17
  else if(outcome=="pneumonia")
    i <- 23
  else
    stop('invalid outcome')
  
  unique.states <- sort(unique(data1$State))
  print(unique.states)
  
  result.df <- list()
  for(state in unique.states)
  {
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
        result.df <- rbind(result.df, list(NA, state))
        next
      }
      else
        num = num
    }
    else {
      stop('invalid num')
    }
    data1.state <- data1.state[order(data1.state[,i], data1.state$Hospital.Name),]
    hnames <- data1.state[num, ]$Hospital.Name
    print(hnames[1])
    print(state)
    result.df <- rbind(result.df, list(hnames[1], state))
  }
  result.df <- as.data.frame(x=result.df)
  colnames(x=result.df) <- c('hospital', 'state')
  result.df
}

head(rankall("heart attack", 4),13)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)


r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
