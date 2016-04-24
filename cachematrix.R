## Programming assignment requiring us to write
## two functions to cache the inverse of a matrix in order to save
## computational time if we have to compute the inverse of the same matrix again.

## The function makeCacheMatrix below creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixInverse <- function(inverse) m <<- inverse
  getmatrixInverse <- function() m
  list(set=set, get=get, setmatrixInverse=setmatrixInverse, getmatrixInverse=getmatrixInverse)
}


## The function cacheSolve below computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix is same), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixInverse()
  
  # Return inverse if it exists
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # Otherwise, compute the inverse
  matrixData <- x$get()
  m <- solve(matrixData, ...)
  x$setmatrixInverse(m)
  m
}
