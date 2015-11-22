# The code below has been crated for following assignment:
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.


# makeCacheMatrix function :
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtr = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtr <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtr);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` 
# should retrieve the inverse from the cache.

cacheSolve <- function(mtr, ...) {
  inverse <- mtr$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtr$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}