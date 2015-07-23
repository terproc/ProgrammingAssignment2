## Two functions to allow for the caching of an inverse matrix by storing
## that inverse matrix in a "special matrix" created by the
## makeCacheMatrix function

## Function to cache the inverse of a matrix; gets and sets the given
## matrix, and gets and sets the inverse of the matrix

makeCacheMatrix <- function(cMatrix = matrix()) {
    iMatrix <- NULL

    set <- function(givenMatrix){
        cMatrix <<- givenMatrix
        iMatrix <<- NULL
    }

    get <- function() cMatrix

    setIMatrix <- function(invMatrix) iMatrix <<- invMatrix

    getIMatrix <- function() iMatrix

    list(set = set, get = get, setIMatrix = setIMatrix, getIMatrix = getIMatrix)
}


## Checks to see if given "special matrix" has a cached inverse matrix;
## if it does, returns that matrix, if not, it calculates the inverse,
## stores it in the "special matrix" and returns the inverse matrix

cacheSolve <- function(x, ...) {
    invMatrix <- x$getIMatrix()

    if(!is.null(invMatrix)){
        message("Retrieving cached inverse matrix")
        return(invMatrix)
    }
    invMatrix <- solve(x$get())
    x$setIMatrix(invMatrix)
    invMatrix
}
