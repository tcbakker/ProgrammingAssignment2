## The functions in this script, makeCacheMatrix and cacheSolve, are used to 
## store and retrieve cached inversed versions of a matrix.

## Documentation on the use of solve to inverse has been found on:
## http://www.endmemo.com/program/R/solve.php

## The makeCacheMatrix function creates a special "matrix": 
## it is a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize inverse
    inverse <- NULL
    
    ## set and get x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    ## set and get inversie
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    
    ## create a list with the created functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    
    ## Try to retrieve cached version
    inverse <- x$getinverse()
    
    ## If it is found, return the cached version
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## Otherwise inverse data, cache it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    ## return inverse
    inverse
}
