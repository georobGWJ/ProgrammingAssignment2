## makeCacheMatrix() is a funtion that creates an enhanced matrix object. 
## It takes a matrix as input and stores that matrix object and the calculated 
## Inverse of that matrix, and provides helper functions to initialize the 
## cacheMatrix, get the matrix and its inverse, and set the matrix and inverse.
## Initially, the Inverse is set to NULL.


makeCacheMatrix <- function(cache_matrix = matrix()) {
        matrix_inverse <- NULL

        # Function to initialize the cache matrix 'object'
        setMatrix <- function(input_matrix) {
                cache_matrix <<- input_matrix
                matrix_inverse <<- NULL
        }

        # Function to return the input matrix
        getMatrix <- function() cache_matrix
        
        # Function to calculate the inverse of the matrix
        setInverse <- function(inverse) matrix_inverse <<- inverse
        
        # Function to return the inverse of the matrix
        getInverse <- function() matrix_inverse
        
        # Return list of functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve() takes a matrix and returns a matrix that is the 
## inverse. It first gets the inverse for the input matrix from its
## cacheMatrix object. It then checks that the cacheMatrix inverse is valid 
## (not NULL) and that the input matrix is the same as the cacheMatrix matrix. 
## If so, it returns the inverse; if not, it calculates the inverse and 
## sets it to the cacheMatrix object.

cacheSolve <- function(input_cache_matrix, ...) {
        
        # Get the matrix inverse from the cacheMatrix
        matrix_inverse <- input_cache_matrix$getInverse()

        # Check that the inverse is not NULL and that the input cache matrix
        # and the internal archived matrix in the cacheMatrix agree.
        # If so, return the already cached matrix.
        if(!is.null(matrix_inverse) && input_cache_matrix == input_cache_matrix$getMatrix()) {
                message("Getting cached data . . .")
                return(matrix_inverse)
        }

        #  Create a temporary cacheMatrix object, calculate and set the inverse
        cache_matrix <- input_cache_matrix$getMatrix()
        matrix_inverse <- solve(cache_matrix, ...)
        input_matrix$setInverse(matrix_inverse)

        # Return Inverse
        matrix_inverse
        
}
