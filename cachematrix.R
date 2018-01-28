## makeCacheMatrix and cacheSolve enable the caching of a matrix and its inverse
## ensuring that the inverse is computed only once

## makeCacheMatirx creates a list that stores the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x 
    setInverse <- function(inverse) i <<- inverse 
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of a makeCacheMatrix matrix 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setInverse(inverse)
    inverse
}
