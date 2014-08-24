## 
## This library contains methods for caching the inverse of a matrix.  
## There are two functions 'makeCacheMatrix' and 'cacheSolve'. 
## The first one can be used to create a matrix object that is cacheable,
## The second one uses a cacheable matrix object and returns the inverse
## of the matrix.
## 

## Returns a matrix object where the inverse can be cached.
## Parameters:
##   x The matrix object that should be used. If omitted, a new
##     empty matrix object is created.
## Returns:
##   A function with the following methods
##     set Can be used to set a new matrix 
##     get Returns the underlying matrix
##     setinverse Sets the inverse of the matrix
##     getinverse Returns the inverse of the matrix if set, otherwise NULL
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Returns the inverse of the given cacheable matrix.
## If the inverse was calculated before, the cached value is returned.
## Parameters:
##   x The cacheable matrix for which the inverse should be calculated
##   All additional parameters are appended to the solve function 
## Returns:
##   The inverse of the given matrix
## It throws an error if the given matrix is not cacheable or not invertible
cacheSolve <- function(x, ...) {
    if (!all(c("getinverse","setinverse","get","set") %in% names(x))) {
        stop("The given matrix is not cacheable")
    }
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
