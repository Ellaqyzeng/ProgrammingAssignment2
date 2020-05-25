# This work is for the assignment2 of R programming course.
# The pair of functions written below are to cache the inverse of a matrix. 
# The reason/benefit to write these functions is to save time and efforts in the computational process of matrix inversion. 


## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}



## This function creates the inverse of the matrix returned by makeCacheMatrix, or retrieve the cache if the inverse has already been calculated. 
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
