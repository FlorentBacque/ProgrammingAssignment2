## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse (assuming the matrix is invertible)

makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL
    
    set <- function(matrix) {
        x <<- matrix
        inverseMatrix <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(solve) {
        inverseMatrix <<- solve
    }
    
    getInverse <- function() {
        inverseMatrix
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, return the inverse of the matrix from the cache

cacheSolve <- function(x, ...) {

    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}