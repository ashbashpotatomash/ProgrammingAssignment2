## This function creates a special "matrix" object that is actually a list of
## functions to set or get the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix created with the function above.
## It first checks to see if the inverse has already been calculated. If so, it 'get's
## the inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and stores in cache via the 'setinverse' function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  i
}




