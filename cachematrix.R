## Pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # store inverse value
        inverse<-NULL
        # set the original matrix and reset inverse
        set <- function(y) {
                x<<-y
                inverse<<-NULL
        }
        # get the original matrix
        get<-function() x
        # set inverse value
        setinverse<-function(inv) inverse<<-inv
        # get inverse value
        getinverse<-function() inverse
        
        # Returns a list of the 4 functions, this list is the special "matrix"
        list(set=set, get=get,
                setinverse=setinverse,
                getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
                setmean = setmean,
                getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}