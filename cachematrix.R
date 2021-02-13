## Nikki McCloud
## R-Coursa Programming Assignment 2
## 2/13/2021

## This algorithm creates a matrix that has the ability
## to cache it's inverse. 

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


## This algorithm takes the inverse from the matrix that
## was a output for the above cacheMatrix algorithm and
## locates the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("cached dataset")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Test Cases
Test1 <- matrix(c(1,2,3,4),2,2)

Test2 <- makeCacheMatrix(Test1)

cacheSolve(Test2)

