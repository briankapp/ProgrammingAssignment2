## Set up a list to hold four functions to get and set a matrix and it inverse and cache them.
makeCacheMatrix <- function(x = matrix()) {
  ## inv will store inverse; reset whenever matrix is updated with set
  inv <- NULL
  
  ## sets the matrix (and clears inverse)
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  
  ## get() will retrieve the matrix  
  get    <- function() x
  
  ## setInv and getInv will compute/retrieve the inverse of the matrix
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  
  # return the list of functions
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
  
  ## Set up a separate variable (data) to hold the matrix; compute and return the inverse 
  data <- x$get()
  inv  <- solve(data)
  x$setInv(inv)
  inv
}
