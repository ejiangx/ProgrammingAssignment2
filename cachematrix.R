## Put comments here that give an overall description of what your
## functions do
#  Two functions, makeCacheMatrix and cacheSolve, act to create a 
#  special matrix and calculate the inverse of the special matrix
#  created, respectively. However, if the inverse has already been
#  calculated, cacheSolve will bypass the computation and retrieve
#  the inverse from the cache.

## Write a short comment describing this function
#  makeCacheMatrix creates a special matrix object that can cache 
#  its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#  cacheSolve computes the inverse of the special matrix returned
#  by makeCacheMatrix. If the inverse has already been calculated
#  (and the matrix has not changed), then the cacheSolve should
#  retrieve the inverse from the cache.
#  The matrix is assumed to be invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
