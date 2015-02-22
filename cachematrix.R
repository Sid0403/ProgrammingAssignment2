## THe functions have been written to compute the inverse of a matrix and cache its value
## so that it can be used again and computation does not need to be done every time.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve              ##Calculating inverse here and caching the value in m
        getinverse <- function() m                             ## by using <<- symbol
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above, or retrieves the inverse of matrix from the cache
## if inverse has already been calculated.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){                                ## Using the if statement to check if the inverse has already been calculated
                message("getting cached data")          ##if true, then directly returns from cache.
                return(m)
        }
        data <- x$get()                                 ##Or else further computation is done to 
        m <- solve(data, ...)                           ##calculate inverse
        x$setinverse(m)
        m                                                ## Return a matrix that is the inverse of 'x'
}
