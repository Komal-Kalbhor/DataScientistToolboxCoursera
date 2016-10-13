setwd("D:\\BI\\Coursera\\Courses\\2R\\week2")
complete <- function(directory,id)
{
  d = NULL
  for(i in id)
  {
    myfiles<-list.files(directory,full.names=TRUE)[i]
    dataframe<-read.csv(myfiles)
    x<-sum(complete.cases(dataframe))
    d = rbind(d, data.frame(i,x))
  }
  d
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
cc$x

cc <- complete("specdata", 54)
cc$x

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "x"])


# complete2("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata",c(2,4,8,10,12)) 
# 
# set.seed(42)
# cc<-complete2("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", 332:1)
# use <- sample(332, 10)
# print(cc[use, "x"])
# print(cc$x)
# 
# set.seed(42)
# cc <- complete2("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", 332:1)
# use <- sample(332, 10)
# print(cc[use, "x"])
# 
# cc <- complete2("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", 54)
# print(cc$x)
# 
# cc <- complete2("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", c(6, 10, 20, 34, 100, 200, 310))
# print(cc$x)
# 
# 
# x <- c(3, 5, 1, 10, 12, 6) 
# x[x %in% 1:5] <- 0
# x[x <= 5] <- 0
# x[x < 6] <- 0
# 
# x
