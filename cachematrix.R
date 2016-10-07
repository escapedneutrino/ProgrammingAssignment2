## Defines functions for cacheable matrices to save time
## on inversion

## makeCacheMarix returns a cacheable version of 
## matrix argument 'x'

makeCacheMatrix <- function(x = matrix()) {
  # TODO: maybe input sanity check for squareness
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <-function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates inverse of cacheable matrix 'x'
## if necessary and stores it so future calls don't have to

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # TODO: maybe check that x is right kind of object
  inv <- x$getinv()
  if(!is.null(inv)){
    return(inv)
  } # Or, if not already cached, calculate then return:
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
