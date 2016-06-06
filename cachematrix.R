## creates a matrix object that can cache its inverse

MakeCacheMatrix <- function(x = matrix()) {  
        inv <- NULL
        ## set the inv variable to NULL as placeholder
        Set <- function (y) {
                ## defines function to set vector and reset inverse to NULL in cache
                x <<- y
                inv <<- NULL
        }
        Get <- function() x
        ## this returns the vector x
        SetInverseMatrix <- function(inverse) inv <<- inverse
        ## function sets inv to inverse
        GetInverseMatrix <- function() inv
        ## returns the inverse
        list(Set = Set, Get = Get,
             SetInverseMatrix = SetInverseMatrix,
             GetInverseMatrix = GetInverseMatrix)
        ## returns function specified above 
}

## this returns a matrix that is the inverse of 'x' from cache, thus computing the identity matrix by matrix x inverse_matrix
CacheSolve <- function(x, ...) {     
        inv <- x$GetInverseMatrix()
        if(!is.null(inv)) {
                message("I am getting cached data")
                return(inv)
        }
        matrix <- x$Get()
        inv <- solve(matrix, ...)
        x$SetInverseMatrix(inv)
        inv
        ## returns inverse matrix 
}
