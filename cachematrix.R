## The functions allow to quantify the inverse of a sqare 
## linear matrix, then store the result in cache. The
## result can be called again without the need to 
## requantify it.

## This function takes a matrix and creates a list, that will 
## store its value and the value of its inverse (after 
## cacheSolve is called)
## It contains functions to get or set 
## the values of the matrix, as well get or set the inverse of
## the matrix from cacheSolve. 
## These functions are called by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function will first evaluate the list generated
## makeCacheMatrix. If the inverse of the matrix is known (i.e.
## stored as getinverse) it will return that value, followed by a 
## message indicating that it is retrieved from cache. If the 
## value is not known, it will quantify it, send to cache and 
## print.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
