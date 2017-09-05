## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## "makeCacheMatrix" fuction store a martix and a cached value of the inverse 
## of the matrix, while "cacheSolve" function checks wether an inverse already
## exist or not, if not then it inverse a matrix.

## This function creates a special "matrix" object that can cache its inverse .

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function which computes the inverse of the special "matrix".
## However, it first checks to see if the inverse has already been calculated.
## Then the cachesolve should return the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
  
