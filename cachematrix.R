## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## matrixinv stores the Inverse of the matrix "x"
    matrixinv <- NULL
    
    ## Get and set functions for the matrix "x"
    set <- function(matrix) {
        x <<- matrix
        matrixinv <<- NULL
    }
    get <- function() x
    
    ## Get and set functions for the matrixinv which stores the 
    ## inverse of matrix "x"
    setmatrixinv <- function(solve) matrixinv <<- solve
    getmatrixinv <- function() matrixinv
    
    list(set = set, get = get,
         setmatrixinv = setmatrixinv,
         getmatrixinv = getmatrixinv)    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Fetch matrix inverse of "x"
    matrixinv <- x$getmatrixinv()
    
    ## if inverse of matrix "x" is cached return it
    if(!is.null(matrixinv)) {
        message("getting cached data")
        return(matrixinv)
    }
    
    ## else calculate it and cache it
    matrix <- x$get()
    matrixinv <- solve(matrix, ...)
    x$setmatrixinv(matrixinv)
    
    ## Return matrixinv that is the inverse of 'x'
    matrixinv
}