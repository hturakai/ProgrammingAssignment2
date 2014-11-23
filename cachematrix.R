## Programming Assignment 2 for R Programming on Coursera


# Creates a special "matrix" object that can cache its inverse.
#
# The input matrix is stored in the function's environment as x
#
# Returns a list of functions to get/set the matrix and the inverse of the matrix. 
#
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  # Set the matrix for the cache. Clear the inverse.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Returns the matrix
  get <- function() {
    x
  }
  
  # Sets the innverse in the cache
  setInverse <- function(newInverse) {
    inv <<- newInverse
  }
  
  # Returns the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Returns the list of functions for manipulating the cache
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve returns the inverse from the cache.
#
# Computing the inverse of a square matrix is done with the R solve function.
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#
# Prerequisite: the matrix must always be invertible.
#
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  # If the inverse is already in the cache, then return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # There is no cached version. Create a new inverse with solve, store it to cache and return it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}


# Unit tests
#

test <- function(name, expected, actual, tolerance = 0.001){
  if(!isTRUE(all.equal(expected, actual, tolerance=tolerance))){
    print(paste("Test", name, "failed"))
  }else{
    print(paste("Test", name, "success"))
  }
}

m_orig <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
m_inv <- matrix(c(0.0, 1, 0.5, 0), nrow = 2, ncol = 2, byrow = TRUE)
m <- makeCacheMatrix(m_orig)
test("matrix", m_inv, cacheSolve(m))
# The second test will show the value came from cache!
test("matrix", m_inv, cacheSolve(m))



