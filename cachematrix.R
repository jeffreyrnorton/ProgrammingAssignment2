## Matrix inversion is expensive.  This set of functions holds a
## square, (assumed) invertible matrix.  It uses lazy evaluation
## such that the inverse of the matrix is stored in the object
## after being calculated the first time.

## makeCacheMatrix
## Create an object with a valid square matrix that can be inverted
## (not checking for singularities or degeneracies)
## If the object cannot be coerced to a proper square matrix, the
## function will fail fatally.
##
makeCacheMatrix <- function(m = matrix()) {
  # Check that m is a matrix
  if(class(m) != "matrix") {
    # Coerce to a matrix if possible
    dim <- sqrt(length(m))
    if( trunc(dim)-dim != 0 ) {
      stop("Cannot invert non-square matrix after coersion")
    }
    # Generate the matrix.
    m <- matrix(data=as.numeric(m),
    nrow=as.integer(dim), ncol=as.integer(dim))
  } else {
    if( dim(m)[1] != dim(m)[2] ) {
      stop("Cannot invert non-square matrix")
    }
  }
  # Return the cacheMatrix.  We fill in the inverse now just
  # for "cleanliness" of the inversion code.
  return(list("matrix"=m, "inverse"=NULL))
}


# cacheSolve
# Return the inverted matrix if cached.  If not, calculate it,
# then return it.
cacheSolve <- function(cacheMatrix, ...) {
  if( is.null(cacheMatrix$inverse) ) {
    # Calculate the previously uncalculated inverted matrix.
    cacheMatrix$inverse <- solve(cacheMatrix$matrix)
  }
  cacheMatrix
}

