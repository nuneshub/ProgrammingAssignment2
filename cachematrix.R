################################################################################
## 
## R Programming - Assignment 2 (Final version)
## Caching the Inverse of a Matrix
## 
################################################################################

## makeCacheMatrix creates a list containing functions to:
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(matinv) {
                inv <<- matinv
        }
        getinverse <- function() {
                inv
        }
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve returns a matrix that is the inverse of 'x'
## Checks to see if the inverse matrix has already been calculated and, in 
##      this case, it returns the cached data.
## Assumption: matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


