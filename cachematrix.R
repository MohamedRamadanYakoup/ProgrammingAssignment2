## these functions get a square matrix and then 
## cache its inverse because its computationly expensive


## Input: matrix
## this function to load and cache the data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function get the inverse of the matrix
## and check if it cached or not to return the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
          message("you get cached inverse")
          return(inv)
        }
        
        data_inv <- x$get()
        inv <- solve(data_inv, ...)
        x$setinv(inv)
        inv
}
