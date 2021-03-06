## The pair of functions makeCacheMatrix and cacheSolve
## cache the inverse of a matrix, so that once inverse of
## a matrix is calculated, it's retrieved from the cache
## subsequently, but only until the main matrix is changed

## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  # set inv and mChange to initial values
  inv <- NULL
  mChange <- TRUE
  
  # define a function which can be used to set special matrix quantities
  set <- function(y) {
    m <<- y
    inv <<- NULL
    mChange <<- TRUE
  }
  
  # define get a function to return the special matrix
  get <- function(){ m}
  
  # define a set inverse function that sets the inverse and mChange
  setinv <- function(inverse){
    inv <<- inverse
    mChange <<- FALSE
  }
  
  # define a get inverse to return the inverse
  getinv <- function(){inv}
  
  # define a function to return mChange value
  isMChange <- function(){mChange}
  
  # return functions as a list
  list(set = set, get = get
       , setinv = setinv
       , getinv = getinv
       , isMChange = isMChange)
  
}


## cacheSolve function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the
## matrix has not changed), then it retrieves the inverse
## from the cache. If the matrix has been changed it recalculates the inverse


cacheSolve <- function(x, ...) {

  # get the inverse using getinv method from makeCacheMatrix
  inv <- x$getinv()
  # Ask whether the matrix has changed or not
  isMC <- x$isMChange()
  
  # If the cached inverse is not NULL and
  # if the matrix has not changed
  if(!is.null(inv) && !isMC) {
    message("Getting cached inverse of matrix")
    return(inv)
  }
  # Get the current matrix for which inverse
  # has been requested using get method from makeCacheMatrix
  m <- x$get()
  
  # Calculate matrix inverse allow additional arguments to be passed
  inv <- solve(m, ...)
  # Set the calculated inverse in data structure using setinv method
  x$setinv(inv)
  
  message("Getting fresh calculated inverse of matrix")
  inv
  
}
