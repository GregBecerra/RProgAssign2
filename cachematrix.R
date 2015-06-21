## Programming Assignment 2 - R Programming

## The following two functions cache the inverse of a matrix. 
## Matrix inversion is usually a costly computation and 
## there may be some benefit ot caching the inverse of a matrix 
## rather than compute it repeatedly.


## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    # get current state
    inv <- x$getinverse()

    # if state null compute, otherwise continue
    if(is.null(inv)) {
	  message("Generating matrix...")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
    }
    else {
        message("Using cached matrix...")
    }

    # return current state
    return(inv)
}
