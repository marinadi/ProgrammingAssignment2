## makeCacheMatrix is a function that creates a cache of a given matrix  's
##inverse

makeCacheMatrix <- function(x = matrix()) {
      matrix_inverse <- NULL
      set <- function(y) {
            x <<- y
            matrix_inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) matrix_inverse <<- inverse
      getinverse<- function() matrix_inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)     
}

## function computes the inverse oof the  matrix or if already calulated, 
## retrieve this information from cache
cacheSolve <- function(x, ...) {
      
      matrix_inverse <- x$getinverse()
      if(!is.null(matrix_inverse)) {
            message("getting cached data")
            return(matrix_inverse)
      }
      matx <- x$get()
      matrix_inverse<- solve(matx, ...)
      x$setinverse(matrix_inverse)
      matrix_inverse
}