## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is a costly computation and it would be preferable 
## to cache the inverse of a matrix rather than compute it repeatedly.
## The pair of functions below create a special object which stores the matrix and generates its inverse
## caching it for repeated use in the future.

## Write a short comment describing this function
## This function makes a matrix object which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  set <- function(y) {
    result <<- NULL
    x <<- y
  }
  get <- function() x
  setInverse <- function(inverse) result <<- inverse
  getInverse <- function() result
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This is the caching function. Checks first whether the inverse has already
## been computed, if so returns the inverse. If not, it computes the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result <- x$getInverse()
  if (!is.null(result)) {
    message("extracting the data that we cache")
    return(result)
  }
  todo <- x$get()
  result <- solve(todo, ...)
  x$setInverse(result)
  result
}
