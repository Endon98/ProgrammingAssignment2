## This Script contains two functions: makeCacheMatrix accepts a matrix as input
## and returns a "special" matrix.

## This function accepts a matrix as input and creates a "special" matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inv <<- solve
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This accepts our "Special" matrix of the previous function and returns the 
## inverse of the matrix entered into makeCacheMatrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
