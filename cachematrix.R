## An R function that is able to cache potentially time-consuming computations.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( trix = matrix() ) {

    i <- NULL

    set <- function( matrix ) {
            trix <<- matrix
            i <<- NULL
    }

    get <- function() {
	trix
    }

    setInv <- function(inverse) {
        i <<- inverse
    }

    getInv <- function() {
        i
    }

    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {

    trix <- x$getInv()

    if( !is.null(trix) ) {
            message("getting cached data")
            return(trix)
    }

    data <- x$get()

    trix <- solve(data) %*% data

    x$setInv(trix)

    trix
}
