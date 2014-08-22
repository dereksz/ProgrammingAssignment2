## CacheMatrix - a performance improved matrix inverter

## Example Usage:
##    dim <- 1000
##    mx <- matrix(rnorm(dim*dim), c(dim,dim))
##    mc <- makeCacheMatrix(mx)
##    system.time(r1 <- cacheSolve(mc)) ## just under 5 seconds on my machine
##    system.time(r2 <- cacheSolve(mc)) ## basically zero, because it will return the cached result

## makeCacheMatrix creates a wrapper object that will house the underlying matrix
## and provides accessor methods for setting and getting the matrix, and for
## getting and setting the inverse.
##
## As a user of this function, you should ONLY be calling the setter!
## Request the inverted matix via cacheSolve, and the creation and caching
## will be delt with under the convers.

makeCacheMatrix <- function(x = matrix()) {
  ## cache of inverted mx is initially empty
  inv <- NULL
  ## Setter function to set the contained mx
  set <- function(y) {
    x <<- y ## save the new mx
    inv <<- NULL ## and get rid of any old cached version
  }
  ## Simple getter function
  get <- function() x
  
  ## "private" methods - use cacheSolve instead of calling these directly
  ## Setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  ## getter for the inverse
  getinv <- function() inv
  
  ## returns "object" with the four "methods" defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve works on a CacheMatrix created with makeCacheMatrix, above,
## and can obtain the inverse of the underlying mx, using a cached copy
## if the inverse has previously been requested.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## If not NULL, cache has been created and we can simply return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise get the underlying mx
  data <- x$get()
  ## calc the inverse
  inv <- solve(data, ...)
  ## store it into the cache for future callers
  x$setinv(inv)
  ## return it to our caller
  inv
}
