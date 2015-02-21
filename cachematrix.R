## A pair of functions the cache the inverse of a matrix.
## 
## 'makeCacheMatrix' and 'cacheSolve' 
##
## Supply 'makeCacheMatrix' with the matrix to be processed.
## Use 'cacheSolve' to return the matrix inverse.
##
##  Calling the functions:
##
##  	matInput <- matrix(c(1:4), nrow=2, ncol=2)
##  	mat1 <- makeCacheMatrix(matInput)
##
##  	mat1Inv <- cacheSolve(mat1)
	

## makeCacheMatrix:
##
## A function that ...
##	... takes a matrix as its argument
##	... returns a list of functions that enable functions to:
##		 retrive and cache the matrix, 
##		 calculate its inverse, 
##		 cache and retreive its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {


  	MtrxInv <- NULL

	## Set up functions to ...
	##
	## ... setMtrx: cashe the input matrix and erase the cache for its inverse
	
    	setMtrx <- function(y) {
        	x <<- y
        	MtrxInv <<- NULL
    	}

	## ... getMtrx: returns the caches matrix
	
    	getMtrx <- function() x

	## ... setMtrxInv: cashe the matrix inverse
	
    	setMtrxInv <- function(matinverse) MtrxInv <<- matinverse

	## ... getMtrxInv: retrive the cashed matrix inverse
	
    	getMtrxInv <- function() MtrxInv
    	
	## Return a list of functions that manage the matrix and its inverse.

	list(setMtrx = setMtrx, 
	     getMtrx = getMtrx,
             setMtrxInv = setMtrxInv,
             getMtrxInv = getMtrxInv)

}


## cacheSolve:
##
## A function that computes the inverse of the special "matrix" returned
## by 'makeCacheMatrix'. 
##
## It detects whether the inverse has already been calculated for this matrix. 
## 
## If it has, it retrieves the inverse from cache. Otherwise it computes the
## inverse, caches it, and returns the inverse.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix 'MtrxInv' that is the inverse of 'x'

	## Test whether the matrix inverse has previously been calculated and cached.
	##
	## ... call the function to get the contents of the variable 'MtrxInv'
	##     holding the matrix inverse
	##
	## ... if this variable is not empty, the matrix inverse has
	##     previously been cached and is returned. 
	
 	MtrxInv <- x$getMtrxInv()

    	if(!is.null(MtrxInv)) {
        	message("getting cached data")
       		return(MtrxInv)
    	}

	## The matrix inverse has yet to be calculated and cached...
	##	
	## ... retrieve the matrix,
	## ... calculate its inverse
	## ... cache the matrix inverse
	## ... display the inverse and return.
	
    	Mtrx <- x$getMtrx()
    	MtrxInv <- solve(Mtrx, ...)
    	x$setMtrxInv(MtrxInv)
    	MtrxInv
}
