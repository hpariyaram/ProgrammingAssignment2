## This library has two R functions:
##	1. makeCacheMatrix(x)
## 	2. cachSolve(x, ...)
## --------------------------------------------------------------------
##
##	Function:  makeCacheMatrix(x)
##		Takes a matrix as a parameter and creates a list of
##		functions that can be revoked on that matrix.
##		The function makeCacheMatrix() returns a list of 
##		functions:	get() - to get the cached matrix
##				set() - to set a value to the matrix
##				setim() - to set the inverse matrix
##				getim() - to get inverse of the matrix passed
## --------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

	## sets the cache with the matrix inverse.
	set <- function(y) {
	          x <<- y
	          m <<- NULL
		}

	## Initialise the sub-functions and 
	## set the cache with inverse of the matrix.
	get <- function() x

	setim <- function(solve) m <<- solve

        getim <- function() m

	## Forms the list of functions/methods to return.
        list(set = set, get = get, setim = setim, getim = getim)
}


## --------------------------------------------------------------------
##
##	Function:  cacheSolve(x, ...)
##		Takes a matrix as a parameter and returns the 
##		inverse of the matrix.
## 	Example: The matrix "mymat" is passed to the cacheSolve() and
##		the function returns the inverse of the matrix as below:
##	> mymat
##		[,1] [,2]
##	[1,]    1    3
##	[2,]    2    4
##	> cacheSolve(mymat)
##		[,1] [,2]
##	[1,]   -2  1.5
##	[2,]    1 -0.5
##	> 
## --------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## Invoke the cache and if the 
	## inverse matrix is already in cache or not?
	mylist <- makeCacheMatrix(x)

        m <- mylist$getim()

        if(!is.null(m)) {
             message("getting cached data")
             return(m)
        }

	## If the cache is empty find the inverse matrix and caches it.
        data <- mylist$get()

	m <- solve(data, ...)

	mylist$setim(solve)

	## Returns the inverse matrix either invoked or calculated.
        m
}
