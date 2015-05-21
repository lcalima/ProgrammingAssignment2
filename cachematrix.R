## Below are two functions that attempt to create 
## a special object that stores a square matrix 
## and cache it's inverse. Matrix inversion is 
## usually a costly computation and caching the 
## inverse of a matrix is preferable to computing
## it repeatedly.

## The first function, makeCacheMatrix, creates a 
## square matrix, which is really
## list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = inverse)
}

## cacheSolve computes the inverse of the square 
## matrix returned by the preceding makeCacheMatrix.
## If the inverse has already been calculated and the matrix
## unchanged then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
