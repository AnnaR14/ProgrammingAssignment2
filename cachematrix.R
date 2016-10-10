## ProjectAssignment2 is to write a pair of functions that cache 
## the inverse of a matrix

## This function creates an R object that stores a maxtrix and cache 
## its inverse using the Solve function

makeCacheMatrix <- function(x = matrix()) {
        ## initialize x (row above) and s (row below)
        s <- NULL
        ## assign input argument to x and set x to NULL in parent environment
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## retrieves x from parent environment
        get <- function() x
        ## assign input argument to s in the parent environment to 
        ## complete function
        setsolve <- function(solve) s <<- solve
        ## retrieves s from parent environment
        getsolve <- function() s
        ##assign each function as an element within a list(), and return 
        ## it to the parent environment.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Uses an argument returned by makeCacheMatrix() in order to retrieve
## the inverse from the cached value that is stored in the makeCacheMatrix().


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
               if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
