## A pair of functions that cache the inverse of a matrix rather than compute it repeatedly

## example for test

# x <- rbind(c(1, -1/4), c(-1/4, 1)) 
# specialmatrix<-makeCacheMatrix(x)
# xinverse<- cacheSolve(specialmatrix)


## This function creates a special "matrix" object that can cache its inverse.
## Return a list of 4 functions
makeCacheMatrix <- function(x = matrix()) {
  
  
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setsolve <- function(inverse) inv <<- inverse
  # get the value of the inverse
  getsolve <- function() inv
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getsolve()
  
  #If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
 
  # if the inverse is not calculated then compute the inverse
  # and sets the value of the inverse in the cache via the setsolve function.
  data <- x$get()
  # we assume that the matrix supplied is always invertible.
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
