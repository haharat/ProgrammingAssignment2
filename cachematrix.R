# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # inv indicates the inverse of a given square matrix
    # initialize inv with NULL value 
    inv <- NULL
    
    # this function sets the matrix x with given values from y
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # this function returns the values of x
    get <- function() x
    
    # this function saves the calculated inverse value into inv
    setinv <- function(inverse) inv <<- inverse
    
    # this function returns the value of inv
    getinv <- function() inv
    
    # combine functions' returns
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix.
cacheSolve <- function(x, ...){
    # get the current value of inv
    inv <- x$getinv()
    
    # If the inverse has already been calculated (and the matrix has not changed), 
    # then the cachesolve should retrieve the inverse from the cache.
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # If the inverse has not been calculated,
    # then do the calculation.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
