## Function to create a special matrix object

makeCacheMatrix <- function(m = matrix()) {
  minv <- NULL
  set <- function(mx) {
    m <<- mx
    minverse <<- NULL
  }
  get <- function() m
  setinverse <- function(inv) minv <<- inv
  getinverse <- function() minv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function that returns the inverse of a matrix from the cache if it exists in cache
## Else compute the inverse using solve and return the inverse matrix

cacheSolve <- function(m, ...) {
  minv <- m$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- m$get()
  minv <- solve(data, ...)
  m$setinverse(minv)
  minv
}
