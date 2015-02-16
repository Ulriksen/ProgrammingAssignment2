## This module contains two methods which enables caching of the inverse of a matrix

## makeCacheMatrix takes a matrix and returns a function which 
## use closures and module pattern to support caching of solve operation
## The solve is stored in an internal variable m. 
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

##Takes a cacheMatrix object solves the matrix. If the matrix is already solved
##the result will be returned from the cached value.
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}