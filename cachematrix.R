## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setInverse <- function(getInverse) invs <<- getInverse
    getInverse <- function() invs
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getInverse()
    
    # If matrix inversion exists, get cached data
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    
    # Calculate matrix inversion if no cached data
    message("Calculating data")
    data <- x$get()
    invs <- solve(data)
    x$setInverse(invs)
    invs
    
}
