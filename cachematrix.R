## The intention of this program is to return the inverse of a matrix.
## Either from cache (if it's calculated before), or it's gets calculated
## To do this we need two functions

## The makeCacheMatrix creates a list with 4 objects, that is input for the second funtion (CacheSolve)

makeCacheMatrix <- function(x = matrix()) {                 #x is the input
        m <- NULL                                           #the cached result is initialized to NULL
        set <- function(y) {                                # set function,used when x is reinitilazed
                x <<- y
                m <<- NULL
        }
        get <- function() x                                 # one line function that stores x
        setinv <- function(solve) m <<- solve               # one line function that sets m to the inverse
        getinv <- function() m                              # one line function that stores m
        list(set = set, get = get,                          # returning argument, a list of 4 objects
             setinvn = setinv,
             getinv = getinv)
}


## This second functions returns the inverse, using the output of makeCacheMatrix

cacheSolve <- function(x, ...) {                            # x is the input
        m <- x$getinv()                                     # retrieve the cached value   
        if(!is.null(m)) {                                   # if cached value is not NULL then return it
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                     # if cached value is NULL then matrix is retrieved
        m <- solve(data, ...)                               # its inverse is calculated
        x$setinv(m)                                         # and stored
        m                                                   # returns the resulting inverse matrix
        ## Return a matrix that is the inverse of 'x'
}
