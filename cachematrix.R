## makeCacheMatrix takes a matrix as input and returns a makeCacheMatrix
## object.  This returned object contains functions that allow the
## original matrix to be:
##    set: - replaced with a new matrix
##    get: - returned
##    setinverse: - an inverse matrix to be set
##    getinverse: - an inverse matrix to be returne
## This object works in conjunction with cacheSolve.
## 
## Note: cacheSolve must be called prior to getinverse as this is
## written.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function () m
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve takes an object created by makeCacheMatrix
## as input and returns the inverse of the matrix within
## the input object
##
## It will check for a cached value of the inverse and 
## return this if it exists.  If the inverse does not exist, 
## it will generate the inverse using "solve" and store this 
## value making it available for future cached retrievals.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
