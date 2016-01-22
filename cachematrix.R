## R Progamming Assignment 2: Lexical Scoping

## The following two functions are designed to cache a computation to
## avoid time-consuming recalculating when calling on the value.

## These functions work in tandem to a) create a special object that
## stores a numeric matrix and b) cache's its inverse.

## This first function, makeCacheMatrix creates a matrix list object with functions:

##      1) sets the values of the matrix
##      2) gets the inverse of the matrix
##      3) sets the values of the matrix
##      4) gets the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

        invrs <- NULL
        set <- function(y) {
                x <<- y
                ## Note": "<<-" is an operator used to assign a value to an object
                ## in an environment that is different from the current evironment
                        
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invrs <<- solve
        getinverse <- function() invrs
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## This second function calculates the inverse of the special "matrix" object created
## with the makeCacheMatrix above. It first checks to see if the inverse has already
## been computed. If it has, it gets the inverse from the cache and skips the rest of the
## computation.  Otherwise it solves for the inverse of the matrix and sets the values of
## the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinverse(invrs)
        invrs

}