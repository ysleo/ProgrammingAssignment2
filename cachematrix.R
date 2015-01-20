## The goal of these functions is to create a kind of object
## to store matrix and the inverse matrix associated
## 

## makeCacheMatrix create an object (a list) 
## it contains 2 variables and the associated setters/getters
makeCacheMatrix <- function(x = matrix()) {
	# cache variable
	inverted_matrix <- NULL
    # function that set the matrix
    # param y : matrix we need to inverse 
    setmatrix <- function(y) {
		matrix <<- y
		# new matrix is set so erase the cache
		inverted_matrix <<- NULL
	}
	# function to retrieve matrix
	getmatrix <- function() x
	# function to store the inverse matrix
	setinvertedmatrix <- function(p_inverted_matrix) inverted_matrix <<- p_inverted_matrix
	# function to retrieve the inverse matrix
	getinvertedmatrix <- function() inverted_matrix
	# list that represent the object cache 
    list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinvertedmatrix = setinvertedmatrix,
             getinvertedmatrix = getinvertedmatrix)
}


## cacheSolve take in parameter the object (list) created by 
## makeCacheMatrix. Retreive the 
cacheSolve <- function(x, ...) {
	## Get inverse matrix from object 
	m <- x$getinvertedmatrix()
	if(!is.null(m)) {
		## The result has been previously cache
		## return it 
		message("getting cached data")
        return(m)
	}
	currentmatrix <- x$getmatrix()
	## Return a matrix that is the inverse of 'x'
    m <- solve(currentmatrix, ...)
	## Cache the result in the object
    x$setinvertedmatrix(m)
	m
}
