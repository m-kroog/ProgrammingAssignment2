## makeCacheMatrix and cachesSolve work in conjunction with each other
## to take a matrix, invert it and cache for retrieval
## this will eliminate the process and time needed to invert the same matrix again

## makeCacheMatrix creates a function with x as its argument, then sets the value of m to NULL
## as an empty matrix
## another function called set is created and assigns its argument y to x
## in its parent environment, m is also set to NULL in its parent environment
## the next function displays lexical scoping by retrieving the value of x
## from the parent environment
## the next function runs the function solve on the value m from the parent environment
## the next function retrieves the value m from the parent environment
## the final part names all of the functions to allow extraction in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve creates a function with x as its argument that assigns the value of getsolve
## to m, which is either the cached inverted matrix or a NULL value
## the if statement returns the value of m if m is not NULL
## if the value of m is NULL then the function will take the matrix in m and invert it
## using the solve function and set it to m in the parent environment

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}