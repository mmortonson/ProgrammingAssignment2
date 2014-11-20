## Create a matrix object that caches the matrix inverse.
## The function makeCacheMatrix defines the matrix object 
## and its associated functions. The function cacheSolve
## returns the cached inverse if it has already been computed,
## and otherwise computes a new inverse and caches it.

## makeCacheMatrix creates an object that stores a matrix and
## caches its inverse. It returns the following list of functions:
##     - set: store a matrix in the object
##     - get: return the stored matrix
##     - setInverse: cache a given matrix inverse
##     - getInverse: return the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(newInv) {
        inv <<- newInv
    }
    getInverse <- function() {
        inv
    }
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of a given matrix object
## created with makeCacheMatrix. If the inverse of the matrix
## has already been computed, the cached inverse will be returned;
## otherwise, the inverse will be computed and cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse.")
    } else {
        inv <- solve(x$get())
        x$setInverse(inv)
    }
    inv
}
