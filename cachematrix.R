# Functions to instantiate a custom matrix supporting cached inversion, as well as computing said inversion.


# Create a new cached matrix from a regular matrix.
# Input:
# x - any matrix
# Output:
# A cached matrix, used with cacheSolve, to compute the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# Compute the inverse of a cached matrix, utilising cached value if available.
# Input:
# x - cached matrix created with makeCacheMatrix()
# Output:
# the inverse of the input matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
