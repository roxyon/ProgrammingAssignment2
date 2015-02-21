## These two functions allow storage and retrieval of the inverse of a matrix in the parent environment
## of the functions by caching the inverse in the parent environment and retrieving that value instead 
## of the value in the current environment of the function being called

## This function returns the list of functions that operate on the matrix, being  
## set = resets the value of the matrix to the one in the parent environment and the inverse to null, 
## get = gets the matrix, 
## setsolve = sets the inverse(i) in the parent environment to the inverse of the matrix passed,
## getsolve = gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checks for a cached value of i, and, if i is not null, returns i.  It then sets data to the matrix x, 
## sets i to the inverse of x, then stores i in the parent environment, caching it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
}
