## makeCacheMatrix takes a matrix as a parameter, which enables the user to get back the input matrix, set the value of the input matrix, set the inverse of the matrix, and get the inverse of the matrix, 
## cacheSolve attempts to get the inverse of a matrix. If the matrix has been cached using makeCacheMatrix, cacheSolve will look for the inverse in the list made in makeCacheMatrix
## If the inverse has already been cached, cacheSolve will quickly return the inverse. If the inverse hasn't been cached, cacheSolve will solve for the inverse, cache the inverse, and return the inverse

## makeCacheMatrix makes a list containing functions to 
    ## 1. set value of the matrix, 2. get value of the matrix, 3. set the value of the inverse, 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve looks for inverse of matrix from the list in makeCacheMatrix and returns the inverse with a message if it finds inverse
## if no inverse is found, the function gets the value of the matrix, solves for the inverse, stores the inverse in the list, and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
