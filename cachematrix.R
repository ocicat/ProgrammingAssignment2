##
## These functions create a wrapper around R's matrix() construct
## to increase the performance of finding the matrix inverse by caching
## its computed value.  This wrapper is implemented by forming a
## closure.  The inverse is also kept in the same environment frame,
## but only computed when required.  As such, this wrapper emulates
## object-oriented programming through encapsulation & establishes
## a limited interface through the exposed functions.
##
## The enclosed matrix inverse is set/reset to NULL when the matrix
## is set.  Whenever the inverse is required (by calling the enclosed
## getinverse() method...), return the cached value.  If the cached
## value is NULL, recompute the matrix inverse, save its value, &
## return the inverse.
##
## eg.
## > m <- rbind(c(1, -1/4), c(-1/4, 1))
## > m
##    [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > o <- makeCacheMatrix(m)
## > cacheSolve(o) %*% m
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > m %*% cacheSolve(o)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## >
##

## Creates a closure around both matrix & inverse values, & returns
## a list composed of access methods.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(m) inv <<- m
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the computed matrix inverse.  If not previously cached,
## compute anew.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinverse(inv)
  inv
}
