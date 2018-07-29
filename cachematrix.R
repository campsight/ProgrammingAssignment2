## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. The pair of functions below 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    cashed_inv <- NULL
    set <- function(y) {
        x <<- y
        cashed_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inversed_matrix) cashed_inv <<- inversed_matrix
    getinverse <- function() cashed_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    x_inversed <- x$getinverse()
    if(!is.null(x_inversed)) {
        message("getting cached data")
        return(x_inversed)
    }
    data <- x$get()
    cashe_inv <- ginv(data, ...)
    x$setinverse(cashe_inv)
    cashe_inv
}
