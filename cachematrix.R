##A pair of methods that can cache the inverse of a matrix

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  get <- function()  m
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## Computes the inverse of the special matrix returned by "makeCacheMatrix()"
## above. If the inverse has already been calculated then the "cachesolve" would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ##returning the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Getting the matrix from object x
  data <- x$get()
  
  ## Calculating inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Setting inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
