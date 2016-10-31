
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invrs <<- inverse
  getinv <- function() invrs
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above.If the inverse has already been calculated , 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  invrs <- x$getinv()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinv(invrs)
  return(invrs)
  
}
