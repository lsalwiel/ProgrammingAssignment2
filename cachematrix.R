## makeCasheMatrix and casheSolve are functions that create a special ##object to store a matrix and cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

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


## Computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If inverse has already been calculated (and the 
## matrix has not changed) it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns matrix inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("retrieve cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
