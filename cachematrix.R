## The functions in this file work in tandem to calculate
## the inverse of a matrix and cache the results.
##
## To start, a square invertible matrix must be
## passed to the makeCacheMatrix function. This 
## function returns a list of functions (essentially 
## emulating a special matrix class) to get and 
## set the value of the passed matrix, and get
## and set the inverse of that matrix. 
## 
## The cacheSolve function can then use this
## special matrix to calculate the inverse of 
## that matrix and cache the results.

## Constructor function that returns a set of functions
## to get and set the value of a matrix, and get and set
## the inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse variable as NULL.
        i <- NULL

        ## Create a function to update the value of
        ## the matrix and reset it's inverse to NULL.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        ## Create a function to get the value of the matrix.
        get <- function() x

        ## Create a function to set the value of the inverse
        ## of the matrix.
        set_inverse <- function(inverse) i <<- inverse

        ## Create a function to get the value of the inverse
        ## of the matrix.
        get_inverse <- function() i

        ## Finally, return the functions that were created
        ## in a named list.
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Function to get the inverse of a matrix created
## though the makeCacheMatrix function. This function
## will calculate the inverse of the matrix once, then
## caches the results. Subsequent calls to calculate the
## inverse return the cached value, unless the value of
## the matrix is updated through the set function of the
## makeCacheMatrix return type. The inverse will then be
## recalculated and recached.
cacheSolve <- function(x, ...) {
        ## Get the inverse of the matrix x.
        i <- x$get_inverse()

        ## If the value of the inverse is not NULL ...
        if(!is.null(i)) {
                ## ... then the inverse has already been calculated.
                message("Getting cached data...")

                ## Return the cached valued and exit the function.
                return(i)
        }

        ## Otherwise, retrieve the value of the matrix x ...
        data <- x$get()

        ## ... then calculate the inverse ...
        i <- solve(data, ...)

        ## ... and cache the results.
        x$set_inverse(i)

        ## Finally, return the inverse of the matrix x.
        i
}
