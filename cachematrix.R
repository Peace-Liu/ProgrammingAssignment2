## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse;
## It contains 4 function: 1) set the value of matrix, 2) get the value of matrix,
## 3) set the inverse of matrix, 4) get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ## assume x is a squared invertible matrix
  I<-NULL
  set <- function(y) {
    ## use `<<-`  to assign a value to an object in an environment that is different from the current environment
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) I <<- solve
  getInverse <- function() I
  
  ## return the list including set/get matrix, set/get inverse as the input of cacheSolve()
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## x is output of makeCacheMatrix()      
  I <- x$getInverse()
  
  # if the inverse has already been calculated
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  # otherwise, calculates the inverse
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  
  ## Return inverse of matrix of 'x' which is the input of makeCacheMatrix
  I
}
