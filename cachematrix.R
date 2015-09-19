## Assignment 2
## Alejandro Rivero
## Construct special matrix to compute inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse<<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Compute the inverse of a square matrix using the data saved
## in cache if available.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getsolve() 
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  ##Compute elapsed time
  startTime <- Sys.time()
  inverse <- solve(data, ...)
  endTime <- Sys.time()
  time <- endTime - startTime
  print(time)
  x$setsolve(inverse)
  inverse
}
