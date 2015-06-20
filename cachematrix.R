## Special vector used to cache the inverse matrix

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


## Return a matrix that is the inverse of 'x' via the cache if it has been previous calculated.

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
