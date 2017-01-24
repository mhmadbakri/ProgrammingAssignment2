## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly, hereunder
## we will create two functions to accomplish this.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmat <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setmat = setmat, getmat = getmat,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmat()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}