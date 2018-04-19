## This is a pair of functions that cache the inverse of a matrix

## The "makeCacheMatrix" creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The "cacheSolve" function computes the inverse of a special 
## "matrix" returned by the function above

cacheSolve <- function (x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
    inverse
}
