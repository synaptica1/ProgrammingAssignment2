## The following two functions define and compute the inverse of a matrix, 
## and this value is then cached.

## makeCacheMatrix defines the matrix and the get and set functions

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {					## sets x equal to y and inv equal to null 
                x <<- y					## only after searching parent environments		
                inv <<- NULL					## for existing values
        }
        get <- function() x  					## retrieves the input matrix
        setinverse <- function(inverse) inv <<- inverse	## sets inv equal to inverse if not already defined
        getinverse <- function() inv				## retrieves cached inverted matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix, if one has not already been computed
## or returns the already-computed inverse, if it has previously been cached

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
        if(!is.null(inv)) {					## retrieves cached inverted matrix if it exists
                message("getting cached data")
                return(inv)
        }
        data <- x$get()					## computes inverse of matrix and caches as inv
        inv <- solve(data, ...)				
        x$setinverse(inv)
        inv							## returns inv, the inverted matrix
}


