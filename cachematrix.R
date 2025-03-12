## These functions are used to calculate and cache the inverse of a given matrix 

## Cache the matrix

makeCacheMatrix <- function(x = matrix()) {
      matrix <- NULL
      set <- function(y) {
        x <<- y
        matrix <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) matrix <<- solve
      getInverse <- function() matrix
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Calculates and return a matrix that is the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
      matrix <- x$getInverse()
      if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix) ## Return a matrix that is the inverse of matrix 'x' if already cached
      }else
      data <- x$get()
      matrix <- solve(data, ...) ## Calculates a matrix that is the inverse of matrix 'x'
      x$setInverse(matrix)
      matrix
}
        
