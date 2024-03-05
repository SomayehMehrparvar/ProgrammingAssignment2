## Creates a special matrix object that can cache its inverse.
## The object returned by this function has the ability to store a matrix and its inverse,
## and provides functions to set the matrix, retrieve the matrix, and compute its inverse.

## This function creates a special "matrix" object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y  
    cache <<- NULL  
  }
  get <- function() {
    x  
  }
  getInverse <- function() {
    cache  
  }
  setInverse <- function(inverse) {
    cache <<- inverse  
  }
  list(set = set,
       get = get,
       getInverse = getInverse,
       setInverse = setInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)  
  }
  
  mat <- x$get()  
  inverse <- solve(mat, ...)  
  x$setInverse(inverse)
  inverse  
  
}

