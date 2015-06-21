## Together these two functions calculate the inverse of a matrix and cache it
## for future use

## Takes a matrix, stores it in memory and returns a list of functions:
## set(y): stores y matrix in memory, replacing previous data
## get(): returns stored matrix
## setinverse(inverse): stores inverse in memory
## getinverse(): returns stored inverse matrix

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
} 


## Takes an object created with makeCacheMatrix and returns the inverse
## of the matrix supplied to makeCacheMatrix, reading from cache if available 
## and calculating it and storing it in cache if not.

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
