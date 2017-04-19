## Programming Assignment 2: Caching the Inverse of a Matrix
## Writing a pair of functions that cache the inverse of a matrix.

## This first function is creating a special "matrix" object, aka that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    InverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        InverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) InverseMatrix <<- inverse
    getInverse <- function() InverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}   

## This second function computes the inverse of the special "matrix" returned by
## the first function makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InverseMatrix <- x$getInverse()
    if(!is.null(InverseMatrix)) {
        message("getting cached inverse")
        return(InverseMatrix)
    }
    MyMatrix <- x$get()
    InverseMatrix <- solve(MyMatrix, ...)
    x$setInverse(InverseMatrix)
    InverseMatrix
}
