## These functions will cache a square matrix along with it's inverse
## Assumes matrix are always invertible

## This function caches the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(s) m <<- s
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the matrix is already cached or not:
## if it IS NOT yet cached - it does the inversion of the matrix and caches it
## if it IS already cached - it just retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
  g <- x$getinverse()
  if(!is.null(g)) {
    message("getting cached data")
    return(g)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}