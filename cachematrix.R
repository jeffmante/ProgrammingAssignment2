## caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        j <- NULL
        set <- function(y) {
                x <<- y
                j <<- NULL
        }
        get <- function() x
        setinv <- function(solve) j <<- solve
        getinv <- function() j
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

## At the moment giving: Error in x$getinv : $ operator is invalid for atomic vectors
cachesolve <- function(x, ...) { 
        j<- x$getinv()
        if(!is.null(j)) {
                message("getting cached data")
                return(j)
        }
        data <- x$get()
        j<- solve(data, ...)
        x$setinv(j)
        j
        
}
