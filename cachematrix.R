## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
##of a matrix rather than compute it repeatedly. The following pair of functions cache the inverse of a 
##given matrix.
## This function, makeVector creates a special "matrix", which is really a list containing a function to
##set the value of the matrix,get the value of the matrix,set the value of the inversed matrix,get
##the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x 
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse ,
             getInverse = getInverse )

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
	i
}
