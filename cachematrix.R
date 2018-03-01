#For those who review my script, thank you for your attention :)

#The "makeCacheMatrix" create a special "matrix" object that can cache its inverse.
#The "makeCacheMatrix" store a list containing a function to:
#     1st - Set the value of the matrix;
#     2nd - Get the value of the matrix;
#     3rd - Set the value of the "inversible" matrix, and;
#     4th - Get the value of the "inversible" matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    get <- function() x 
    setinverse <- function(solve_inverse) inverse <<- solve_inverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by previous "makeCacheMatrix" function.
#If the inverse has already been calculated and the matrix has not changed, then the cacheSolve should
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    getinverse <- x$get()
    inverse <- solve(getinverse, ...)
    x$setinverse(inverse)
    inverse
}
