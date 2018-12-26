
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix  <- function(x = matrix() ) {
   m <- NULL
   set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix 
  
  getInverseMatrix  <- function() m
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve  <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data for inverse of matrix")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
}
