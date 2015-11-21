## The following functions demonstrates how to add a inverse
## matrix to the cache and reuse it
## if the inverse is there, the value will be fetched from the
## cache, else it will be added to the cache after computing the
## In addition, I have add a check to make sure the matrix is add
## square matrix as inverse can be computed only for square matrix

## Usage: Example if a inverse for a matrix b<-matrix(c(4,2,7,6),nrow=2,ncol=2)
## a<-makeCacheMatrix(b)
## cacheSolve(a)
## Result 
##    [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4

## This function makeCacheMatrix caches the matrix
## Sets the value of the value of matrix in the cache
## Sets the value of the value of the inverse of the matrix
## get the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

	# Assign the target inverse matrix as NULL
	m<-matrix()
	
	# set the matrix to the cache and initializes the inverse
	        set <- function(y) {
                x <<- y
                m<<- matrix()
        }
	
	# Return the original matrix
	get <- function() x
	
	# Set the inverse of the matrix in the cache
	setinv <- function(invmatrix) m <<- invmatrix
	
	# Get the inverse of the matrix
	getinv <- function() m

	 # returns the list that can be operated 
	 # This can be used in the operation by the matrix provided
	 # as input
	 list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Cachesolve calulates and returns the inverse if the data is not in cache
## Else returns the inverse matrix from the cache
## Following validations are performed in this function
## If the input matrix is not a square matrix, the function return empty matrix

cacheSolve <- function(x, ...) {

	# Make sure the input matrix is a square matrix. Else inverse will fail
	
	r<-nrow(x$get())
	c<-ncol(x$get())
	
	if ( r != c ) {
		message(" Inverse can be calculated only for square matrix")
		return(matrix())
	}
	# get the inverse of the matrix
	m <- x$getinv()
	
	## Check if the inverse exists in cache
	## Use complete.cases that return true vector
	## If even one value returns TRUE, the data exists	
	## I am ignoring NA matrix here. What is a point
	## in storing a NA matrix in cache
	
	if (any(complete.cases(m))) {
		message("getting cached data")
        return(m)
	}
	
	# get the original matrix that is passed
	data<-x$get()

	## get the determenant
	detvalue<- det(data)
	## compute the inverse
	
	invdata<-solve(data)
	## set the inverse in the cache
	m<-x$setinv(invdata)
	
	### return the value of the inverse matrix
	m
}

