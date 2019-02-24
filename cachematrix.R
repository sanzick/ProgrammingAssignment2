## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly. 

## The two functions below ("makeCacheMatrix" and "cacheSolve") will cache the inverse of a matrix.
## Assume that the matrix supplied is always invertible.

## The makeCasheMatrix function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # set the value of the matrix
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        ## get the matrix
        get <- function() x
        ## set the inverse of the matrix
        set_inverse <- function(inverse) inv_matrix <<- inverse
        ## get the inverse of the matrix
        get_inverse <- function() inv_matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.  If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$set_inverse(inv_matrix)
        inv_matrix
}