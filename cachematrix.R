## Receives a matrix as argument and 
## creates a matrix object able to to store its cache.
makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        cached_inverse <<- inverse
    }
    getinverse <- function() {
        cached_inverse
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix object returned by makeCacheMatrix()
## If the inverse was already calculated, and the matrix was not changed,
## it returns the cached result.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversed_m <- x$getinverse()
    if(!is.null(inversed_m)) {
        message("returning inversed matrix from cache")
        return(inversed_m)
    }
    data <- x$get()
    inversed_m <- solve(data)
    x$setinverse(inversed_m)
    inversed_m
}
