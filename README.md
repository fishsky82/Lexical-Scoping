## These functions work together to either solve for the inverse of a
## matrix or to retrieve the inverse if it is already stored in memory.

## makeCacheMatrix creates a four step vector of functions for each scenario
## that cacheSolve requires to store and retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## Set Matrix in Cache to Null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get Matrix
  get <- function() x
  
  ## Set Inverse in Cache
  setinverse <- function(inverse) m <<- inverse
  
  ## Get Inverse from Cache
  getinverse <- function() m
  
  ## Return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of a given matrix or
## retrieves the existing result from cache.

cacheSolve <- function(x, ...) {
  ## If m is not null return the cache data
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise calculate the matrix inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ## And then store the results in the cache
  x$setinverse(m)
  
  ## Return the newly calculate inverse of the matrix
  m
}
Status API Training Shop Blog About Pricing
 
 
