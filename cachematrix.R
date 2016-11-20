## Matrix inversion is expensive.  This set of functions holds a
## square, (assumed) invertible matrix.  It uses lazy evaluation
## such that the inverse of the matrix is stored in the object
## after being calculated the first time.

# makeCacheMatrix
# Create an object with a valid square matrix that can be inverted
# (not checking for proper dimensions or singularities)
makeCacheMatrix <- function(mat=matrix()) {
  
  # Initialize the matrix "m" and its inverse "inverse"
  inverse <- NULL
  
  # Set the matrix
  set <- function(m) {
    mat <<- m
    inverse <<- NULL
  }
  
  # Get the matrix
  get <- function() mat
  
  # Set the inverse matrix (from cacheSolve ideally)
  setInverse <- function(inv) inverse <<- inv 
  
  # Get the inverse
  getInverse <- function(cacheMatrix) inverse

  # Return the cacheMatrix.  We fill in the inverse now just
  # for "cleanliness" of the inversion code.
  return(list(get=get, set=set, setInverse=setInverse, getInverse=getInverse))
}

# cacheSolve
# Return the inverted matrix if cached.  If not, calculate it,
# then return it.
cacheSolve <- function(cacheMatrix) {
  if( is.null(cacheMatrix$getInverse()) ) {
    # Calculate the inverse
    message("Calculating inverse")
    inv <- solve(cacheMatrix$get())
    cacheMatrix$setInverse(inv)
  }
  cacheMatrix$getInverse()
}
