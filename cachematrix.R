## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	##set function() to set the matrix to compute.
  	set <- function(y) {
      		x <<- y
          	m <<- NULL
	    }
	##get function() to get the matrix being used.
	
	get <- function() x

	##setInverse function to set the inverse of the matrix.

	setInverse <- function(Inverse) m <<- Inverse

	##getInverse function() to get the Inverse of the matrix.

	getInverse <- function() m

	list(set = set, get = get,setInverse = setInverse,
	getInverse = getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	Inverse <- x$getInverse()
	
	##conditon to check is Inverse of the matrix is already computed and stored.
	
	if(!is.null(Inverse)) {
		message("getting cached data")
	        return(Inverse)
	}
	data <- x$get()
	Inverse <- solve(data)
	x$setInverse(Inverse)
	Inverse
}
