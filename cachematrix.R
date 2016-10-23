## These functions work together to make special 
## matrix objects of which the inverse can be 
## cached. 

## This function will take a matrix x and use its 
## contents to create a "cache matrix". This is
## a matrix that has four functions for, getting
## and setting the value of the matrix, and for
## getting and setting the value of the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will return the inverse of
## cache matrix object defined above. It will
## use the solve method if the inverse hasn't 
## been calculated yet. If the inverse has been 
## calculated and the value of the cache matrix
## didn't change then the inverse will be
## retrieved from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(! is.null(i)){
    message("Getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
