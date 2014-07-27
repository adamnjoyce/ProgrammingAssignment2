## A pair of functions that cache the inverse of a matrix

## Creates an object with 4 function elements allowing for the setting of a matrix,
## the returning of the matrix, the caching of the matrix inverse,
## and the returning of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL				#creates variable to hold inverse
        set <- function(y) {			#changes the matrix and resets the inverse
                x <<- y
                i <<- NULL
        }
        get <- function() x			#returns the matrix
        setSolve <- function(solve) i <<- solve	#sets the inverse
        getSolve <- function() i		#resturns the inverse
        list(set = set, get = get,		#stores all makeCacheMatrix information in a list
             setSolve = setSolve,
             getSolve = getSolve)
}

## Retrieves a possible cached inverse and checks its validity,
## then returns or calculates, caches and returns the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getSolve()			#sets variable to cached inverse value
        if(!is.null(i)) {			#checks for cached inverse, returns if valid
                message("getting cached data")
                return(i)
        }
        data <- x$get()				#retrieves matrix
        i <- solve(data, ...)			#computes inverse
        x$setSolve(i)				#caches inverse
        i					#returns inverse
}
