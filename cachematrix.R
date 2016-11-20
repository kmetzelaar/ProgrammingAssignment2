## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) { ## Return a "special" matrix object that caches the inverse of matrix 'x'
        
        inverse_var <- NULL ## init a variable to carry the inverse
        
        ## The following four sub-functions will be combined in a list that is returned by
        ## by makeCacheMatrix. The list is the "special" matrix object.
        
        set <- function(y) { 
                x <<- y ## take an input matrix and set it as the new source matrix
                inverse_var <<- NULL ## initiaize inverse variable since there is a new source
        }
        
        get <- function() x ## display the contents of the source matrix
        
        set_inverse <- function(inverse_func) inverse_var <<- inverse_func ## take an input matrix and set it as the inverse
        
        get_inverse <- function() inverse_var ## display the contents of the inverse matrix
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        ## return the list as the "special" matrix object
}



cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        ## If the inverse has already been calculated (and the matrix has not changed), 
        ## then cacheSolve should retrieve the inverse from the cache.
        
        inverse_var <- x$get_inverse() ## pull the inverse from the list created by makeCacheMatrix
        if(!is.null(inverse_var)) { ## if the inverse matrix exists, return it
                message("getting cached data")
                return(inverse_var)
        }
        
        matrix_var <- x$get() ## create a temporary working variable to carry the source matrix
        
        inverse_var <- solve(matrix_var, ...) ## calculate the inverse matrix
        
        x$set_inverse(inverse_var) ## store the inverse matrix in the list
        
        inverse_var ## return the inverse matrix
}
