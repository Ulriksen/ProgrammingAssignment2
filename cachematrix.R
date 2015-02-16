## This module contains two methods which enables caching of the inverse of a matrix

## makeCacheMatrix takes a matrix and returns a function which use 
## closures and a list of functions to support caching of solve operation.
## This follows the module pattern. 
## The solve is stored in an internal variable m. 
## Calling set will clear the cached value. The 
## cahced value can be retrived using getSolve and set using setSolve.
## to retrieve the matrix, call the get function in list. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(
    set = set, 
    get = get,
    setSolve = setSolve,
    getSolve = getSolve)

}

## Takes a list created by the makeCacheMatrix and returns an inverse matrix.
## If the matrix is already solved it will be returned from cache. If not 
## the result will be calculated using solve on the matrix in the list from
## the makeCacheMatrix instance.
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if (!is.null(m)) {
      message("getting cached solve")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}