## These two functions use lexical scoping to cache the inverse of matrix

## How to use
## cachevector <- makeCacheMatrix - to create list of functions
## basematrix - the base matrix whose inverse is to be cached)
## cachevector$set(basematrix) - set the matrix in environment
## cacheSolve(cachevector) - will solve, cache, and return the inverse


## The first function creates a special vector of four functions used to
## cache the inverse of the matrix

makeCacheMatrix <- function(mtrx = matrix()) {
  inv <- NULL
  set <- function(y){
    mtrx <<- y
    inv <<- NULL
  }
  get <- function() mtrx
  setsolve <- function(nvrs) inv <<- nvrs
  getsolve <- function() inv
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This second function uses the makeCacheMatrix vector to get the inverse
## (if already solved) or calculates it if not

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  mtrxdata <- x$get()
  inv <- solve(mtrxdata)
  x$setsolve(inv)
  inv
}
