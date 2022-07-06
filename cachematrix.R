## Matrix inversion is a costly computation, here we define two functions which
## cache the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse. 

## This matrix object is really a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(matrix, ...) {
    ## Return a matrix that is the inverse of 'matrix'
    inverse <- matrix$getinverse()
    ## if inverse retrieved from cache then it is returned
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## if inverse cannot be retrieved from cache, it is calculated using solve()
    data <- matrix$get()
    inverse <- solve(data, ...)
    matrix$setinverse(inverse)
    inverse
}
