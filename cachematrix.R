# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. The 
# following functions can be used to cache the inverse of a matrix.

MakeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse.
  #
  # Args:
  #   x: A matrix.
  #
  # Returns:
  #   A list containing functions to get and set the value of the matrix
  #   and functions to get and set the value of the inverse of the matrix.
  inv <- NULL
  SetMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  GetMatrix <- function() {
    x
  }
  SetInverse <- function(i) {
    inv <<- i
  }
  GetInverse <- function() {
    inv
  }
  list(SetMatrix = SetMatrix, GetMatrix = GetMatrix,
       SetInverse = SetInverse, GetInverse = GetInverse)
}

CacheSolve <- function(x, ...) {
  # Returns the inverse of the matrix. If the matrix object already has 
  # it's inverse cached, then this function returns it. Otherwise, it 
  # computes the inverse and caches the result.
  # This function assumes that the matrix supplied is always invertible.
  #
  # Args:
  #   x: An invertible matrix.
  #
  # Returns:
  #   The inverse of the matrix.
  inv <- x$GetInverse()
  
  if (!is.null(inv)) {
    message('getting cached data...')
    return(inv)
  }
  
  m <- x$GetMatrix()
  inv <- solve(m)
  x$SetInverse(inv)
  inv
}
