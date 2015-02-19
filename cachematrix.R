## This script caches the inverse of a matrix and retrieves or calculates ## the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	  m <- NULL

       ## Resets matrix and initializes its inverse.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

       ## Gets the matrix.
        get <- function() x

       ## Sets the inverse.
        setinverse <- function(inverse) m <<- inverse

       ## Gets the inverse.
        getinverse <- function() m

       ## Returns the list of functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	  ## Checks if the inverse has already been calculated.
	  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	  ## Get matrix.
        data <- x$get()

	  ## Calculates matrix inverse.
        m <- solve(data, ...)

	  ## Sets new inverse.
        x$setinverse(m)

      ## Returns inverse of matrix.  
       m
}
