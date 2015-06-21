## This functions is used to cache the inverse of a 
## matrix to reduce computation load

## This function creates the special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(m) {
      x <<- m
      inv <<- NULL
    }
  
  get <- function() return(x)
  setinv <- function(i) inv <<- i
  getinv <- function() return(inv)
  return(list(set = set, get = get, 
              setinv = setinv, 
              getinv = getinv))

}


## Return a matrix that is the inverse of 'x' via 
## the cache if it has been previous calculated

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
