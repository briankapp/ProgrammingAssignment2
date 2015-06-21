## Set up a list to hold four functions to get and set a matrix and it inverse and cache them.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  get    <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## If the inverse of the matrix has been computed/cached, return the cached value. Otherwise, compute, cache, and return.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  ## Check to see whether inv has been computed already. Note that inv is set to NULL when x$set is called.
  if (!is.null(inv)) {
    return(inv)
  }
  # Could probably collapse the next few lines into one or two.
  data <- x$get()
  inv  <- solve(data)
  x$setInv(inv)
  inv
}
