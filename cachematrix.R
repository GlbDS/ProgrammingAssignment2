## The goal of above functions is to compute and cache the inverse of an assumed square
## invertible matrix 

## Usage
##
## Initialize a matrix m
## m<-matrix(1:4, nrow = 2, ncol=2)
## Initialize cacheMatrix object with previous m
## myMat <- makeCacheMatrix(m)
## Compute the inverse of myMat
## cacheSolve(myMat)
## -> should display the inverse of m
## Try to compute the inverse a 2nd time
## cacheSolve(myMat)
## -> should retrieve cache value of the inverse
##

## makeCacheMatrix
##
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    # Initialization of inverse Matrix
    invM <- NULL
    
    # Function to set the value of a matrix
    set <- function(value) {
        x <<- value
        invM <<- NULL
    }
    
    # Function to get the value of a matrix
    get <- function() x
    
    # Function to set the inverse matrix of x
    setInverse <- function(value){
        invM <<- value
    }
    
    # Function to get the inverse matrix of x
    getInverse <- function() invM
    
    # Define the list of above functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve
##
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # Does the inverse already exist ?
    invM <- x$getInverse()
    if (!is.null(invM)) {
        message("Recovering cache inverse matrix")
        return(invM)
    }
    
    message("Computing inverse matrix and caching it")
    data <- x$get()
    invM <- solve(data)
    x$setInverse(invM)
    invM
}
