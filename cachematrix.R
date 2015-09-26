## The below functions calculates the inverse of a matrix. 
## First time the inverse is calculated and is cached. 
## And next time a request for inverse comes in, 
##    the cached value is returned instead of recalculating the inverse


## The function makeCacheMatrix creates a special "vector" object
##                              that is a list having four functions.
## The special vector has four functions getMatrix, 
##              setMatrix, getInverse and setInverse which are self explanatory

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getInverse <- function() inverse
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the special "matrix"
##                                               returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed),
##         then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Returning cached data")
        return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}
