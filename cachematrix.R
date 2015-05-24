## The following functions are used to compute and cached the inverse of matrix

## this funciton creates a list of 4 functions: set, get, setinv, getinv
## to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #cache the inverse matrix
	set<- function(y) {
        	x <<- y
        	m <<- NULL #initialize
	}
	get <- function() x #return the input matrix
	setinv <- function(inv) m <<- inv #set the inverse matrix
	getinv <- function() m #return the inverse matrix
	list(set = set, get = get, 
	     setinv = setinv,
	     getinv = getinv)

}


## this funciton computes the inverse of the matrix
## if the inverse has already been calculated, skip the computation and return the result.
## if not, calculate the inverse matrix and set the result in the cache
cacheSolve <- function(x, ...) {
        m <- x$getinv() #get the inverse matrix
	if(!is.null(m)){ #if the inverse has already been calculated
        	message("getting cached data")
        	return (m)
	}
	data <- x$get() #if not already been calculated
	m <- solve(data,...) #solve to get the inverse
	x$setinv(m) #set the result to the object
	m
}
