pollutantmean <- function(directory,pollutant,id)
{
  setwd(directory)
  totalmean <- 0
  print(id)
  for(i in id)
  {
    if(i<10)
    {
      print(i)
      myfile <- paste0("00",i,".csv")
    }
    else if(i>=10 & i<=99)
    {
      print(i)
      myfile <- paste0("0",i,".csv")
    }
    else
    {
      print(i)
      myfile <- paste(i,"csv")
    }
    mydata <- read.csv(myfile)
    mymean <- mean(mydata[,pollutant],trim = 0,na.rm = TRUE)
    totalmean = totalmean + mymean
  }
  totalmean <- totalmean / length(id)
  totalmean
}

##pollutantmean("specdata","nitrate",70:72)
