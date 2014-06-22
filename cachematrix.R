## This collection of functions allows a matrix and its
## inverse to be cached.

## This function creates a list of 4 functions that are
## used to cache the matrix and its inverse.  The 4 functions
## are get and set functions for both the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## The inverse of the matrix is stored in inv.  On the
    ## creation of this cached matrix, the inverse is set
    ## to NULL, meaning the inverse must be calculated
    inv <- NULL
    
    ## The set() function changes the value of the matrix, and
    ## resets inv to NULL, meaning the inverse must be recalculated
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## The get() function gets the value of the matrix
    get <- function() x
    
    ## The setinverse() function sets the value of the inverse.
    setinverse <- function(solve) inv <<- solve
    
    ## The getinverse() function gets the value of the inverse.
    getinverse <- function() inv
    
    ## This is a list of all the functions that comprises this
    ## construct.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## This uses an input cached matrix and outputs the inverse.
## It also sets the inverse of the input matrix.  It only
## calculates the inverse if inv == NULL, signalling the
## inverse has not been calculated.

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
