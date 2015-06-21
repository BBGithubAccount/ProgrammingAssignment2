## The following function returns a vector which stores the matrix and the inverse of the matrix
## and provides functions to retrieve the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
matrixInverse <- NULL
## set the matrix to the input parameter and it's inverse to null (Initialize)
set <- function(mat){
        x <<- mat
        matrixInverse <<- NULL
}
get <- function() x
setinverse <- function(inverse) matrixInverse <<- inverse
getinverse <- function() matrixInverse
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) { ## x is the matrix returned by makeCacheMatrix
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("Getting cached inverse matrix")
                return(inverse) # helpful to add explicit return statement to draw attention to exiting the function
        }
        # if we reach here cache inverse matix was not found, get the matrix and calculate the inverse
        matrix <- x$get
        inverse <- solve(matrix, ...)
        # set the inverse so it can be retrieved from cache the next time and return the inverse
        x$setinverse(inverse)
        inverse
}