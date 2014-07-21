## The functions defined in this module provide support for caching
## the calculation of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

    ## Wrap a matrix into an object that can cache its inverse.

    ## The matrix can be set and retrieved with the get and set
    ## method, and the cached inverse with the the getinverse and
    ## setinverse methods.

    cache <- NULL
    list(
        set=function(y) { x <<- y; cache <<- NULL },
        get=function() { x },
        setinverse=function(value) { cache <<- value },
        getinverse=function() { cache }
    )
}

cacheSolve <- function(x, ...) {

    ## Return the inverse of a matrix wrapped by makeCacheMatrix.

    ## The calculation of the inverse matrix is performed only if no
    ## cached value is available.

    result <- x$getinverse()
    if(!is.null(result)) {
        message("getting cached data")
    } else {
        result <- solve(x$get(), ...)
        x$setinverse(result)
    }
    result
}
