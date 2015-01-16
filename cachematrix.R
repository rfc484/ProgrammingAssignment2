## Creates a special matrix and caches its inverse.

## Creates a special matrix that is a list of functions that 
## set and get the original matrix and set and get its inverse.
## The matrix is assumed to be invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a cached matrix.  The first time the function is called,
## the inverse is computed using the solve function.  After that, a cached inverse 
## matrix is returned.    

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
