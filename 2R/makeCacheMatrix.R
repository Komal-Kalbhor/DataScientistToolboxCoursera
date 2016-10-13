makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


x <- c(7, 4, 9, 3)  
y <- matrix(data = x, nrow = 2, ncol = 2) 
z <- makeCacheMatrix(y)             
inv_mat <- cacheInverse(z)             
inv_mat               
