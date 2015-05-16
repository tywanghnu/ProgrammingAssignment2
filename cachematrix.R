## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.
## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.

## Use makeCacheMatrix(your_matrix) to creat a list of objects and assign it to
## an new object, say "obj".
## Use obj$set(your_matrix) to set up the matrix of which you want to evaluate
## the inverse matrix if you want to change the value of your_matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invx) inv <<- invx
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Use cacheSolve(obj) to get or calculate and cache the inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'your_matrix'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
