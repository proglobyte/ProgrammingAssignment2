## An R function that is able to cache potentially time-consuming computations.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    get <- function() {
	m
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()

    m <- solve(data) %*% data

    x$setInverse(m)

    m
}
