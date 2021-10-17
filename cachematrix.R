## These set of functions cache the inverse of matrix to avoid repetitive computation.


## The makeCacheMatrix function creates a matrix object that caches the inverse.
## This function assumes that the matrix entered is invertible.

makeCacheMatrix <- function(x = matrix()) {
        inverse_cache <- NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                inverse_cache <<- NULL
        }
        ## Get the value of the matrix
        get <- function() {
                x
        }
        ## Set the value of the inverse
        setinverse <- function(inverse) {
                inverse_cache <<- inverse
        }
        ## Get the value of the inverse
        getinverse <- function() {
                inverse_cache
        }

        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the cache matrix returned by the 'makeCacheMatrix' function.
## If the inverse has already been calculated (provided the matrix values have not changed) then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse_cache <- x$getinverse()
        if (!is.null(inverse_cache)){
                message("getting cached data")
                return(inverse_cache)
        }
        data <- x$get()
        inverse_cache <- solve(data, ...)
        x$setinverse(inverse_cache)
        inverse_cache
}
