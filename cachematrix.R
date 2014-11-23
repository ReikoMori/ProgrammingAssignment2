## A pair of functions that cache the inverse of a matrix - 11/24/2014 by R.M.
## two functions: makeCacheMatrix, cacheSolve

## makeCacheMatrix : Function to create a special matrix object

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                               # Initialize the cache for inverse matrix
        setMatrix <- function(y) {　　　        # setMatrix : 1st function to set matrix
                x <<- y
                i <<- NULL
        }
        getMatrix <- function() x               # getMatrix : 2nd function to get matrix 
        setInverse <- function(inverse) i <<- inverse 
        # setInverse : 3rd function to set inverse
        getInverse <- function() i              # getInverse : 4th function to get inverse
        list(setm = setMatrix, getm = getMatrix,     #list for the functions (names differ)
             seti = setInverse,
             geti = getInverse)
        
}


## cacheSolve : Function to to compute inverse for matrix object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$geti()                          # Get inverse cache and set to i
        if(!is.null(i)) {                      # If i is not null, 
                message("getting cached inverse matrix")    # write message,
                return(i)                      # and return it.
        }
        message("setting cache with inverse matrix")
        matrix <- x$getm()                     # If i is null, get matrix
        i <- solve(matrix, ...)                #   set inverse of matrix
        x$seti(i)                              #   and set it to i
        i                                      # Print i 
}
