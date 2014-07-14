## The pair of functions makeCacheMatrix and cacheSolve
## cache the inverse of a matrix, so that once inverse of
## a matrix is calculated, it's retrieved from the cache
## subsequently, but only until the main matrix is changed

## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  mChange <- TRUE
  set <- function(y) {
    m <<- x
    inv <<- NULL
    mChange <<- TRUE
  }
  get <- function() m
  setinv <- function(inverse){inv <<- inverse}
  getinv <- function(){inv}
  isMChange <- function(){mChange}
  list(set = set, get = get
       , setmean = setmean
       , getmean = getmean
       , isMChange)
  
}


## cacheSolve function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the
## matrix has not changed), then it retrieves the inverse
## from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
