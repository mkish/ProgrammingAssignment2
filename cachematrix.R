## The purpose of these functions is as follows:
##      1. calculate the inverse of a matrix
##      2. cache the inverse of a matrix
##      3. return the cached inverse if the matrix does not change
##      4. caculate the inverse if the matrix changed

## The makeCacheMatrix function creates a matrix object and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #initialize variable to hold the inverse matrix
        
        set <-function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix object created 
## by the makeCacheMatrix function. If the inverse has already been computed, 
## then the cacheSolve function will retrieve the cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
