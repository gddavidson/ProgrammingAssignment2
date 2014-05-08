## makeCacheMatrix takes a matrix as input and returns a makeCacheMatrix
## object.  This returned object contains functions that allow the following:
##
##    set: - set a new matrix
##    get: - return matrix
##    setinverse: - set a new inverse matrix
##    getinverse: - return inverse matrix
##
## This object works in conjunction with cacheSolve.
## 
## Note: cacheSolve must be called prior to getinverse or a NULL
## is returned.
##
##  x - matrix
##  m - matrix inverse, NULL until cacheSolve is called

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
  m <- x$getinverse()         ## retrieve cached inverse
  if(!is.null(m)) {           ## if cached inverse exists, return it
    message ("getting cached data")
    return(m)
  }
  ## generate inverse
  data <- x$get()         ## get matrix
  m <- solve(data, ...)   ## calculate inverse
  x$setinverse(m)         ## store inverse
  
  m                       ## return inverse
}
