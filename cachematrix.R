makeCacheMatrix <- function(x = matrix()) {
        # set matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
        # get inverse of the matrix if already been calculated
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # calculate inverse of the matrix 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
