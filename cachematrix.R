## These functions create a special matrix object that can cache
## its inverse. The inverse is calculated only when necessary,
## and the cached inverse is cleared whenever the matrix changes.

## Creates a special matrix object that stores a matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(solve) {
        m <<- solve
    }
    
    getinverse <- function() {
        m
    }
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Computes the inverse of the special matrix object,
## using the cached inverse when it is already available.

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinverse(m)
    
    m
}