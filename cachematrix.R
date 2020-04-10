## These functions take a matrix as input and allow for 
## quicker processing of subsequent calls for intersion of the
## matrix.  
## Use:
## Create a matrix and then call the makeCacheMatrix function on it
## when the inverse is required call the cacheSolve function on the 
## object output from the makeCacheMatrix function


## makeCacheMatrix creates from the matrix a special matrix 
## this special matrix is really a list that stores 
## the matrix as well as the inverse.  The inverse can be 
## quickly recalled for later use by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
  

## cacheSolve takes the list from makeCacheMatrix and 
## checks whether the inverse is stored.  If not, it calculates
## the inverse and stores it.  Then subsequent calls then do
## not need to do the inverse calculation

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
