## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is a list containing
## functions to a) set the value of the matrix, b) get the value of the matrix
## c) set the inverse of the matrix, and d) get the inverse of the matrix.
## This function allows for the inverse of the matrix to be cached for later
## and faster retrieval.
## 
## Note the <<- assignment operator allows for the definitions of the functions
## and variables to be searched for in the parent environment (e.g., not just
## within this function only)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    ## returns a list with the above elements
}


## cacheSolve first searches to see if the inverse of x has been calculated
## and if so, retrieves and returns the inverse. If the inverse of x has 
## not been calculated, the x matrix is retrieved, the inverse calculated,
## the inverse cached, and then the inverse returned.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)        
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    ## Returns matrix that is the inverse of 'x'
}
