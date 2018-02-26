## Functions to cache the inverse of a matrix

## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize variable used for inverse matrix
    i <- NULL
    
    ## Set the value of the new matrix while also
    ## reseting variable for the inverse matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() x
    
    ## Set the value of the inverse
    setInverse <- function(inverse) i <<- inverse
    
    ## Get the value of the inverse
    getInverse <- function() i
    
    ## Return a list of the get/set methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse or returns cache of the 
## special matrix created above
cacheSolve <- function(x, ...) {

    ## Check to see if inverse was previously calculated
    ## If yes, return the inverse from the cache
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Get the uncaluclated matrix
    data <- x$get()
    
    ## Calculate the new inverse
    i <- solve(data, ...)
    
    ## Set the inverse
    x$setInverse(i)
    
    ## Return the inverse matrix
    i
}
