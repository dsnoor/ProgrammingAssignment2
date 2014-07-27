## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
## The following two functions 'makeCacheMatrix' and "cacheSolve" are used to cache the inverse of a given matrix.


## This function creates and returns a new "matrix" object that can cache its inverse.
makeCacheMatrix <- function(myMatrix = matrix()) 
{
	invMatrix <- NULL
        
	# set function clears 
	set <- function(y) 
		{ 	
			myMatrix  <<- y
			invMatrix <<- NULL
 		}

	# returns matrix object 
	get <- function() myMatrix

	# save the inverse to the cache
	setInverse <- function(inverse) invMatrix <<- inverse
	
	# return the cached inverse
	getInverse <- function() invMatrix

	list(	set = set, 
		get = get,
       		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the cached inverse exists return inverse from cache else compute and cache it. 

cacheSolve <- function(myMatrix, ...) 
{
	## Return a matrix that is the inverse of 'myMatrix' from cached, if exists
	inverse <- myMatrix$getInverse()

	if (!is.null(inverse)) 
	{
		message("getting cached data")
		return(inverse)
	}

	# computing the inverse, cache it and also return it.
  	data <- myMatrix$get()

  	inverse <- solve(data, ...)

  	myMatrix$setInverse(inverse)

  	inverse
}

