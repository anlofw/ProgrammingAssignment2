## Caching the inverse of a Matrix

## The functions makeCacheMatrix and cacheSolve can be used to create a "matrix"
## that can cache its inverse. This way the cached inverse can be returned instead
## of performing the usually costly matrix inversion calculation every time.
## We assume that the matrix supplied is always invertible.

## This function creates a "matrix" that can cache its inverse.
## A list with four functions for operating on the matrix is returned:
## set    - set the matrix
## get    - get the matrix
## setinv - set the inverse of the matrix
## getinv - get the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
    ## The inverse is by default not calculated
    i <- NULL

    ## Set the matrix to 'y'
    set <- function(y) {
        m <<- y
        i <<- NULL
    }

    ## Get the matrix
    get <- function() m

    ## Set the inverse
    setinv <- function(y) i <<- y

    ## Get the inverse
    getinv <- function() i

    ## Return the "matrix", which is a list of the functions for manipulating it
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse (using solve()) of a matrix created with the makeCacheMatrix function.
## It first checks if the inverse has already been calculated and returns it instead of calculating it.
## Otherwise, it calculates the inverse of the matrix and cache it using the setinv function of the matrix.

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'm'

    ## First check if a cached result exist and return it if it does
    i <- m$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Otherwise, calculate, store and return the inverse
    data = m$get()
    i <- solve(data, ...)
    m$setinv(i)
    i
}
