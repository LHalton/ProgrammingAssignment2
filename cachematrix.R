## The pair of function create a cache the inverse of a matrix rather than the costly computation of matrix inversion.
## functions do

## Function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Creates the inverse of the special matrix from "makeCacheMatrix". If the inverse has already been calculated, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
