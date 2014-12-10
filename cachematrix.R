# ----------------------------------------------------------------------------------------------------------- #
#  Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of   #
#   a matrix rather than computing it repeatedly                                                              #        
#  This R function cache the inverse of a matrix that is able to cache potentially time-consuming computation #
# ----------------------------------------------------------------------------------------------------------- #


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        
        inv    ## Return a matrix that is the inverse of 'x'
}


# Test

my_matrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
my_matrix$get()
cacheSolve(my_matrix) 
my_matrix$getInverse() 
cacheSolve(my_matrix)

my_matrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
cacheSolve(my_matrix)
my_matrix$get()
my_matrix$getInverse()
