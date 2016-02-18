## This code implements caching for matrices. It's done through 2 functions:
##      - makeCacheMatrix
##      - cacheSolve


## This function initiates matrix and 4 methods
## for reading/writing matrix and reading/writing its inverse

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invx <<- inverse
    getinverse <- function() invx
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function checks if inverse already exists in the cache and
## if positive, provides it's cached version, otherwise - calculates new one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invx <- x$getinverse()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data,...)
    x$setinverse(invx)
    invx
}
