# ASSUMPTION : MATRIX USED AS INPUT FOR THE FUNCTION IS ALWAYS INVERTIBLE
# This function creates a special matrix object, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

# This function computes the inverse of the special matrix returned by the above function.
# If the inverse had been calculated previously and the matrix is the same, then this function retrieves the inverse from the cache.
# The function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
