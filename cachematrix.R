## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve() takes a matrix 'input_matrix' and 
## returns a matrix that is the inverse of 'input_matrix'

cacheSolve <- function(input_matrix, ...) {
        
        matrix_inverse <- input_matrix$getInverse()

        if(!is.null(matrix_inverse) && input_matrix == input_matrix$getMatrix()) {
                message("Getting cached data . . .")
                return(input_matrix)
        }
        cache_matrix <- input_matrix$getMatrix()

        matrix_inverse <- solve(cache_matrix, ...)

        input_matrix$setInverse(matrix_inverse)

        matrix_inverse
        
}
