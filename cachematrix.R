## This function creates a special "matrix" object that can cache its inverse.
## It actucally creates a list that contains 4 member functions: 
## set, get, setInv and getInv. it uses <<- assignment operator to 
## assign a value to an object in an environment that is different
## from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
#cache its inverse
        
	xinv <- NULL  ## this is where the result of inversion is stored
	set <- function(y) {
	        x <<- y
	        xinv <<- NULL ## it also initialises xinv to null 
	}       
	get <- function() x 
	setInv <- function(inv) xinv <<- inv 
	getInv <- function() xinv         
	list(set = set, get = get,
	setInv = setInv,
	getInv = getInv)
}



cacheSolve <- function(x, ...) {
	# return a matrix that is the inverse of 'x'
	m <- x$getInv() 
	if(!is.null(m)) { 
		message("getting cached data")
		return(m) 
	}
	data <- x$get() 
	m <- solve(data) 
	x$setInv(m) 
	m 
}
# # Test
# # generate a random square, non-singular matrix
# test <- matrix(runif(16,1,100),4,4)
# # generate the makeCacheMatrix object with this matrix
# testCached <- makeCacheMatrix(test)
# # from now on calculate or retrieve calculated inversion using the cacheSolve function
# 
# testInv <- cacheSolve(testCached)
# testInv <- cacheSolve(testCached)
