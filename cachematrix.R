##Calculating the Inverse for matrix is a costly operation so if the matrix is not changing it's Inverse should be calculated ##once and we should cache the value for future use, the below functions do this job.


##The function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## 1)set the value of the Matrix
## 2)get the value of the Matrix
## 3)set the value of the Inverse
## 4)get the value of the Inverse

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


## The following function calculates the Inverse of the special "Matrix" created with the above function. However, it first ##checks to see if the Inverse has already been calculated. If so, it gets the Inverse from the cache and skips the ##computation. Otherwise, it calculates the Inverse of the matrix and sets the value of the Inverse in the cache via the ##solve() function.

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
