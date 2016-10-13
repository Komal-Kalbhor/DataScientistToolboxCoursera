setwd("D:\\BI\\Coursera\\Courses\\2R\\week2")

pollutantmean <- function(directory,pollutant,id=1:332)
{
  d = NULL
  for(i in id)
  {
    myfiles<-list.files(directory,full.names=TRUE)[i]
    dataframe<-read.csv(myfiles)
    d = rbind(d, dataframe)
  }
  mymean <- mean(d[,pollutant],na.rm = TRUE)
  round(mymean,3)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
# pollutantmean("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata","sulfate",1:10)
# 
# pollutantmean("D:/R/coursera/week2/specdata","nitrate",70:72)
# 
# pollutantmean("specdata", "sulfate", 1:10)
# 
# 
# pollutantmean("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", "nitrate")
# 
# pollutantmean("D:\\BI\\Coursera\\Courses\\R\\week2\\specdata", "nitrate", 70:72)
