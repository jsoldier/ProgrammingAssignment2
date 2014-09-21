## The following two functions makeCacheMatrix and cacheSolve provides a way to cache
## the inverse of a matrix so that it can be reuse many times without re-calculating it
## every time.


## makeCacheMatrix creates a "special" matrix that is cacheable. It returns a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of the "special" matrix created using makeCacheMatrix
## by first checking if the inverse has already been calculated. If it has, then it will just
## return it, otherwise, it will calculate the inverse then return it.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
