## makeCacheMatrix creates a special "matrix",
## which is really a list containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the matrix's inverse
## - get the value of the matrix's inverse


library(MASS) ## ginv() requires the MASS library
## ginv() is the Moore-Penrose generalised inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               ## initialise empty inverse
        set <- function(y) {                    ## set the value of matrix    
                x <<- y
                m <<- NULL 
        }
        get <- function() x                     ## retrieve the matrix
        setinverse <- function(m) m <<- ginv(x) ## set the inverse    
        getinverse <- function() m              ## retrieve the inverse
        list(set = set, get = get,              ## return a list of the above
                setinverse = setinverse,        
                getinverse = getinverse)
}

## The following function calculates the inverse of the special ## "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the setinverse 
## function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                     ## query the cache
        if(!is.null(m)) {                       ## if there's a cached inverse
                message("getting cached data")
                return(m)                       ## retrive it and end function
        }
        data <- x$get()                         ## if not,
        m <- ginv(data, ...)                    ## compute the inverse here
        x$setinverse(m)                         ## save the x's cache
        m                                       ## return the result
}


