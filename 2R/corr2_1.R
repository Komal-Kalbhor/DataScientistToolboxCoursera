setwd("D:\\BI\\Coursera\\Courses\\2R\\week2")
corr <- function(directory,th=0)
{
  d<-vector()
  for(i in 1:332)
  {
    myfiles<-list.files(directory,full.names=TRUE)[i]
    dataframe<-read.csv(myfiles)
    x<-sum(complete.cases(dataframe))
    if(x>th)
    {
      df1<-dataframe$sulfate
      df2<-dataframe$nitrate
      r<-cor(df1,df2,use="complete.obs")
    }
    else
    {
      r<-0
    }
    d[i]<-r
  }
  d<-as.vector(d,mode="any")
  d
}

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

# cr<-corr2("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata",150) 
# head(cr)
# 
# cr <- corr2_1("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", 2000)                
# n <- length(cr)                
# cr <- corr2_1("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", 1000)                
# cr <- sort(cr)
# print(c(n, round(cr, 4)))
# 
# cr <- corr2_1("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", 129)                
# cr <- sort(cr)                
# n <- length(cr)                
# set.seed(197)                
# out <- c(n, round(cr[sample(n, 5)], 4))
# print(out)
# 
# 
# cr <- corr2_1("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata")                
# cr <- sort(cr)                
# set.seed(868)                
# out <- round(cr[sample(length(cr), 5)], 4)
# print(out)
