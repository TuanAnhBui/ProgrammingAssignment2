## This is a programming assignment for the third week of the 
## R programming course on Coursera. There will be two functions
## written in order to compute and cache the inverse of a matrix
## The input is supposed to be invertible.

## This function is made to create a matrix with the inverse can be 
## supplied and used later on

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## This function is meant to get the inverse of a matrix
## in case it is already supplied, otherwise compute it
## and cache it.

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
