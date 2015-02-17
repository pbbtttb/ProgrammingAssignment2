## 02/17
## Functions for R Programming Assignment #3
## The 'makeCacheMatrix' function handles initialization of the cacheMatrix object.
## The 'cacheSolve' function will return the cached inverse of the matrix if it exists.
## Otherwise, it will calculate the inverse an store the value in the cached Matrix object

## 02/17
## This function will create the cached matrix from the input of a matrix
## This function is a list containing the functions to:
##  set the value of the matrix
##  get the value of the matrix
##  set the inverse value of the matrix
##  get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y){
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invers <<- solve
  getInverse <- function() invers
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## 02/17
## This function will attempt to get the cached value for the inverse of x
## If it exists, it will return the cached value
## If it does not exist, the inverse is calculated, and the cached value is stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invers <- x$getInverse()
  if(!is.null(invers)){
    message("getting cached data")
    return(invers) 
  }
  data <- x$get()
  invers <- solve(data, ...)
  x$setInverse(invers)
  invers
}
