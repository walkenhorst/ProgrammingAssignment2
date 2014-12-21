## These functions add caching to the solve function
## so that when the cacheSolve() function is called, 
## it checks to see if a cached matrix inverse is 
## available and will return that in preference to 
## recomputing the matrix inverse.

## Creates a special matrix object that allows caching
## of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Looks to see if the matrix inverse has already been calculated.
## If the matrix inverse has already been calculated, it is returned, 
## else the matrix inverse is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
