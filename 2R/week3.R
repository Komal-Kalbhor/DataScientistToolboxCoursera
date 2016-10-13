library(datasets)
data("iris")
?iris 
iris

y<-iris[grep("virginica", iris$Species),1 ]
round(mean(y))


apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)
mtcars
#split(mtcars, mtcars$cyl)
#mean(mtcars$mpg, mtcars$cyl)
with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

x<-tapply(mtcars$hp, mtcars$cyl, mean)
round(abs(x[1]-x[3]))

