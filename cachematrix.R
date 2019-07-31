## The functions makeCacheMatrix and Cachesolve optimize the creation and 
## the calculation of a matrix and its inverse. It caches the calculation of the inverse
## so it can optimize the computational timing

## The function makeCacheMatrix creates a list that resembles to matrix containing functions
## to set and get the matrix and then to set and get the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<-y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function() s <- solve(x)
        getsolve <- function () s
        list(set=set, get=get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The second function calculates de inverse of the matrix created by makeCacheMatrix.
## It verifies if this calculation was done before and, if so, skips the computation and 
## prints its cached value. Otherwise, it calculates the inverse matrix and caches it.

cachesolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        mat <- x$get()
        s <- solve(mat, ...)
        x$setsolve(s)
        s
}

