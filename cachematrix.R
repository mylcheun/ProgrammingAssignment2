## author: Maurice Cheung
# This function creates a special "matrix" object that can cache its inverse
# also include getter and setter method 
makeCacheMatrix <- function(x = matrix()) { 
   
  mInverse <- NULL
  # set value of matrix to y 
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  # get value of matrix 
  get <- function() x
  # set value of matrix inverse
  setInverse <- function(inv) mInverse <<- inv
  # get value of matrix inverse
  getInverse <- function() mInverse
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
  
  
} 

#This function return the cached value of inverse, if available
# Otherwise it calculate the inverse by calling solve().
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) # compute inverse
  x$setInverse(i) 
  i
}
