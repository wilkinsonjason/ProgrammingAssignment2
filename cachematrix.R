## This program is for caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
	}
	get <- function()x
	setinverse <- function(solve) x <<- x
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has been calculated (and the matrix is unchanged), 
## then the cachesolve should retrieve the inverse.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return (m)
	}	
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

