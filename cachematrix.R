## This function uses <<- operator to set the variable in a global environment
## which is then used to pull out the information from the cache memory

## makeCacheMatrix cache the inverse of a matrix which is passed to the function
## it then returns the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function checks if the inverse value of the matrix passed is
## available in cache memory, if it finds the value then it will return the result 
## directly from cache memory else it will caculate the inverse, store it in cache 
## and return the result.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
