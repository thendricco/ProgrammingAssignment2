## inv stores the Inverse of the Matrix
## The functions:
## set : sets the value of the matrix
## get : gets the value of the matrix
## setinverse : sets the inverse of the matrix
## getinverse : gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the matrix created by makeCacheMatrix
## But it checks to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}