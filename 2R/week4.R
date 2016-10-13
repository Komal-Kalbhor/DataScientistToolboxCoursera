setwd("D:\\BI\\Coursera\\Courses\\2R\\week4\\rprog-data-ProgAssignment3-data")
hd<-read.csv("hospital-data.csv")
dim(hd)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
names(outcome)
outcome$Hospital.Name
head(outcome$Hospital.Name,10)
outcome[776,]
outcome


set.seed(1)
rpois(5, 2)
