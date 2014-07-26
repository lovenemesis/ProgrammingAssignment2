## Two utility functions to provide cache for matrix inverse operation.
## NOTE: the input matrix has to be invertible.

## Create a special matrix with four helper function with cache.

makeCacheMatrix <- function(x = matrix()) {
  ## Initilize cache to NULL
  cache <- NULL
  ## Parsing parameter for setter,reset cache to NULL.
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  ## return x for getter
    get <- function() { 
    x
  }
  ## cache setter to chain with invoked solve function.
  setInverse <- function(solve) { 
    cache <<- solve
  } 
  ## cache getter for inverted matrix, if any.
  getInverse <- function() {
    cache
  }
  ## Create list for funcation calls
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x', use cached result if possible.

cacheSolve <- function(x, ...) {
  ## Try evaluate the cache, return if is not NULL.
  cache <- x$getInverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  ## Otherwise, retrive matrix via getter
  data <- x$get()
  ## Save inverted matrix back to matrix  
  cache <- solve(data, ...)
  x$setInverse(cache)
  ## Return invertied matrix
  cache
}
