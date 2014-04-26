## This source code contains the functions for R programming assignment 2. The two
## functions are used to create a special matrix for caching its inverse.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  # This function caches the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # This function gets the matrix from the cache.
  get <- function() x
  # Here inverse of the matrix is set
  setinverse <- function(solve) m <<- solve
  # Here inverse of the matrix is get
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # inverse of the matrix is retreived from cache
  # Now it is checked if the m is null or not.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # As inverse was not found in cache, so calculate and cache it.
  data <- x$get()
  m <- solve(data, ...) 
  x$setinverse(m)
  m
}
