# makeCacheMatrix creates a list containing a function to set & get the value of the matrix
# and to set & get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function checks if
# the inverse has already been computed before returning the inverse of the matrix. 
# If yes, it grabs the existing result from the corresponding environmental variable.
## run with: 
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## No cache in the first run
## > cacheSolve(m)
## Cached data in the second run
## > cacheSolve(m)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
