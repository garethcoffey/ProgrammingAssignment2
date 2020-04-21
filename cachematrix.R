## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL  ## if any changes are made to the matrix, the inverse is wiped
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function inverts a matrix, making use of a cached version of the 
## matrix if the cached version exists and no changes have been made to 
## the matrix since the cache was done

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## If there is a cached inverted matrix... note that there is protection
    ## against the inverse being based on old data - the set() function in 
    ## makeCacheMatrix automatically nulls the inverse variable, preventing us
    ## from taking an inverse of outdated contents
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
