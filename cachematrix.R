## Matrix inversion is usually a costly computation. The functions cache the inverse of a matrix rather than compute it repeatedly
## Creates a special Matrix which is basically a list containing funcations get(), set(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {

 inverseM <- NULL
        set <- function(y) {
                x <<- y
                inverseM <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverseM <<- inverse
        getinverse <- function() inverseM
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Following function calculates the inverse of the function. It returns the cached inverse of the matrix, else compute it, assign it and then return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseM <- x$getinverse()
        if(!is.null(inverseM)) {
                message("getting cached data")
                return(inverseM)
        }
        data <- x$get()
        inverseM <- SOLVE(data)
        x$setinverse(inverseM)
        inverseM
}
