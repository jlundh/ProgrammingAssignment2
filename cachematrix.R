## A set of two functions to store and display the value of a matrix and also to calculate and display its inverse.
## To eliminate costly calculation the value of the inverse is also cached after its first calculation. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse matrix of the special "matrix" created with the above function makeCacheMatrix.
## If the inverse matrix has been previously calculated the result is retrieved from the cache. Otherwise the inverse 
## is calculated and stored in the cache.

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}