## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makecachematrix is a function that creates a matrix that can cache the
## inverse of its input.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y 
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cachesolve computes the inverse of the matrix for makecachematrix.
## if the computation is already complete, this function will simply retrive it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
          message("Retrieving cached data")
          return (inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
