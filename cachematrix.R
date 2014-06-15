## cachematrix.R contains 2 functions to calculate the inverse of a matrix
## 

## makeCacheMatrix contains 2 setters and 2 getters.  The setters set the value 
## of the matrix and the matrix inverse.
## The getters get the value of the matrix and the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve determines the matrix inverse, but it first checks to see 
## if the inverse has already been calculated, if so it returns the 
## the contents of the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
