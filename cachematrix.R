# The code below has been crated for following assignment:
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.


# makeCacheMatrix function :
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_mtx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtx <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_mtx <<- inverse
  getinv <- function() inv_mtx
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_mtx <- x$getinv()
  if(!is.null(inv_mtx)) {
    message("Getting cached data...")
    return(inv_mtx)
  }
  data <- x$get()
  inv_mtx <- solve(data, ...)
  x$setinv(inv_mtx)
  return(inv_mtx)
}