## caching the inverse of a matrix rather than compute it repeatedly
## is easier. This code is trying to cache an inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(matrix) {
          x <<- matrix
          m <<- NULL
     }
     
     ## this function creates that matrix.
     
     get <- function() x
     
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
     ## with this function, created matrix gets inversed and stored in.
     
}


cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     ## function computes the inverse of the matrix returned by makeCacheMatrix. 
     ## If the inverse has already been calculated.
     ## the cacheSolve should retrieve the inverse from the cache.
     
     data <- x$get()
     inv <- solve(data, ...) %*% data
     x$setinverse(inv)
     inv
}


