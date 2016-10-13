cube <- function(x, n) {
  x^3
}
cube(3)

x <- 1:10
x
if(x > 5) {
  x <- 0
}
x



x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}
y

h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}
formals(h)

library(codetools)
findGlobals(h, merge=FALSE)[['variables']]

f <- function() {
  x + 1
}
findGlobals(f,merge=FALSE)[['variables']]

f2 <- function(x){
  x+1
}
findGlobals(f2,merge=FALSE)[['variables']]

f2 <- function(x2) {
  print(x2)
  g2 <- function(y2) {
    print(y2)
    print( y2 + z2)
  }
  z2 <- 4
  x2 + g2(x2)
} 

z2 <- 10
f2(3)

ls(environment(f2))
get("x2",environment(f2))
