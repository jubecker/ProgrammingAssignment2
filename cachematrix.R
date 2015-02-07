## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## The combination of the functions makeCacheMatrix and cacheSolve can accomplish this.

## example usage:
##  > m <- makeCacheMatrix(matrix(c(1,2,2,1), 2, 2))
##  > cacheSolve(m)
##  computing the inverse
##  [,1]       [,2]
##  [1,] -0.3333333  0.6666667
##  [2,]  0.6666667 -0.3333333
##  > cacheSolve(m)
##  [,1]       [,2]
##  [1,] -0.3333333  0.6666667
##  [2,]  0.6666667 -0.3333333

## Create new cached matrix object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}

## The function computes the inverse of a 'cached matrix' and caches the result.
## Calling the function a second time will return the precomputed result.
cacheSolve <- function(xi, ...) {
    inv <- x$getInv()
    if(is.null(inv)) {
        message("computing the inverse")
        inv <- solve(x$get(), ...)
        x$setInv(inv)
    }
    inv
}
