## Matrix inversion caching helper.
##
## How to run and test those functions:
##
##  create an inversible matrix:
##   > myMatrix <- matrix(c(1, -1, 1, 2), nrow = 2, ncol = 2)
##   > myMatrix
##        [,1] [,2]
##   [1,]    1    1
##   [2,]   -1    2
##
##  create a cacheable matrix object:
##   > cachedMatrix = makeCacheMatrix(myMatrix)
##
##  ask for the inverse using the cacheSolve function:
##   > cacheSolve(cachedMatrix)
##             [,1]       [,2]
##   [1,] 0.6666667 -0.3333333
##   [2,] 0.3333333  0.3333333
##
##  ask again for the inverse. this time, you'll get the cached inverse:
##
##   > cacheSolve(cachedMatrix)
##   getting cached data
##             [,1]       [,2]
##   [1,] 0.6666667 -0.3333333
##   [2,] 0.3333333  0.3333333
##
## This code is based on the makeVector and cachemean code
## provided as part of the programming assignment #2 of the
## R programming course.

## Create a cached matrix object.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Inverse the provided matrix, using a cached value if it
## has already been solved.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}