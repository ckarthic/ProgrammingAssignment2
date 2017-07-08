## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a normal matrix x and returns a list
## of functions that provides get, set, getinverse and setinverse
## utility functions that provide the caching of the inverse of x

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(invx = matrix()) inv <<- invx
	getinverse <- function() inv
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	origmat <- x$get()
	inv <- solve(origmat,...)
	x$setinverse(inv)
	inv
}
