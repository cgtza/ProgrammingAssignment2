##  The following pair of functions i) compute the generalised
##  inverse of a matrix (using the MASS package in R), but ii) first
##  check to see if the inverse has been computed already, and if so
##  returns the result from a cache function

##  This function creates a list, containing a function to
##  set or get the value of the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  
  library("MASS")
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) m <<- ginv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  }


##  This function computes the generalised inverse of the matrix passed to it, using
##  the ginv() function from the MASS package in R UNLESS this has already been computed,
##  in which case the function gets the value from the cache function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
  library("MASS")
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinv(m)
  m
  
}


