## This script contains functions that cache the inverse of a given matrix.
## Computing the inverse for the first time stores the result. Instead of
## computing the inverse each time, the cached result is returned in all
## subsequent function calls. The cache is emptied when the matrix changes.



## 'makeCacheMatrix' is a factory function that creates a matrix object that
## can cache the inverse of a matrix. The matrix and the cache are stored in
## the environment created when the function is called. The function returns
## the getters and setters for the matrix and the cache.The matrix setter
## also clears the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## 'cacheSolve' returns the inverse of a given matrix created by the function
## 'makeCacheMatrix'. If no inverse is cached, the function uses the standard
## function 'solve()' to compute the inverse and store it, otherwise it returns
## the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
