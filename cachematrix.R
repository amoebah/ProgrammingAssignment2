## Caching the inverse of a Matrix:

## The following function calculates the inverse of the special "matrix" created and saves it as
## a function called makeCacheMatrix. 

makeCacheMatrix <- function(x = matrix()) {

  
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_matrix <<- inverse
  getInverse <- function() inv_matrix
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse )
}


## Checks if the matrix has been cached.
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix from the makeCacheMatrix function and caches it.

cacheSolve <- function(x, ...) {
  
  inv_matrix <- x$getInverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  special_matrix <- x$get()
  inv_matrix <- solve(special_matrix, ...)
  x$setInverse(inv_matrix)
  inv_matrix
}
