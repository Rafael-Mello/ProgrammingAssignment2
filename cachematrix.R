## Put comments here that give an overall description of what your
## functions do

## These functions are used to create a special object that stores a 
## matrix and cache's its inverse.



## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    invertedx <- NULL
    
    set <- function(y) {
        x <<- y
        invertedx <<- NULL
    }
    
    get <- function() x
    
    # Calculate the inverse of the matrix 
    
    setinv <- function(solve) invertedx <<- solve
    getinv <- function() invertedx
    
    list(set = set, get = get,
        setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Calculate the inverse of 'x'
    
    invertedx <- x$getinv()
    
    # Check if there is a cached matrix
    
    if(!is.null(invertedx)) {
        message("getting cached data")
        return(invertedx)
    }
    
    # If not, calculate the inverted matrix
    
    data <- x$get()
    invertedx <- solve(data)
    x$setinv(invertedx)
    invertedx
    
}
