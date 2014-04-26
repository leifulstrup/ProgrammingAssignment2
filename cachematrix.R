## These two functions are designed to streamline matrix inversion caclulation and look up

## 04-26-2014 by L. Ulstrup for Coursera, JHU, Data Science, R Programming Class Peer Assessment assignment

## use source("cachematrix.R") to load this into the working R environment

## The first function makeCacheMatrix creates a list of closure functions and state variables
## The second function cacheSolve mimics the solve() function on the 'cacheMatrix' and only calculates the inverse on the first call, stores it, and then looks it up on subsequent calls

## Warning:  these funcctions below assume that the matrix is square and invertible
## Warning:  these functions need to be upgraded to handle more complex error conditions (e.g., sending it an undefined object as an argument)

## Warning: Assumption per assignment 'For this assignment, assume that the matrix supplied is always invertible.'

makeCacheMatrix <- function(x = matrix()) {
	## returns a list object of anonymous functions that access the closure function variables x and theInverse

	## x should be an nxn matrix ('square' matrix) that can be inverted
	theInverse <- NULL ## this is the variable that will maintain state info on whether an inverse has been calculated

	## test if x is defined as an object in case the argument is ommitted or undefined variable
	if(missing(x)) {
		message("error: argument x is missing or an undefined object - confirm spelling")
		return(NULL)
	}

	set <- function(y) {
		x <<- y ## this is where we store the matrix y when this function is assigned to an object
		theInverse <<- NULL
	}
	get <- function() {x}  ## returns the stored state variable x which os the original matrix

	setInverse <- function(anInvertedMatrix) { theInverse <<- anInvertedMatrix} ## this function changes the state of theInverse variable to the result

	getInverse <- function() {theInverse} ## this simply returns theInverse attribute/variable of the closure function

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## returns a list of the functions


}


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	## this function is desgined to be used in conjection with the makeCacheMatrix function
	## this function speeds up matrix inversion by caclulating it on the first call and then looking it up after that

	## Warning: these function has very weak error checking

	invertedMatrix <- x$getInverse()  ## simply invokes the getInverse function associated with x

    if(!is.null(invertedMatrix)) {  ## checking to see if a value has already been computed for the inverted matrix
    	message("getting cached data")
    	return(invertedMatrix)
    }

    data <- x$get() ## this simply retrieves the original matrix

    ## Warning:  very weak error handling around the solve function.  Needs to be upgraded
    invertedMatrix <- solve(data, ...) ## this uses the R function solve that returns the inverse of a square matrix
    ## Warning: I am not doing an testing prior to invoking solve() to make sure it is even invertable (for instance testing that is a square matrix)
   

    x$setInverse(invertedMatrix)  ## this simply invokes the function that changes the state variable for later look up

    return(invertedMatrix) ## I could have just made the last statement the variable but used return to ensure that is what returned

}


## Examples:
## > aMatrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
## > aMatrix
##      	[,1] [,2]
##	[1,]    1    3
##	[2,]    2    4
## > xMatrix <- makeCacheMatrix(aMatrix)
## > cacheSolve(xMatrix)
##  	   [,1] [,2]
##	[1,]   -2  1.5
##	[2,]    1 -0.5
## > cacheSolve(xMatrix)
## getting cached data
##	     [,1] [,2]
##	[1,]   -2  1.5
##	[2,]    1 -0.5

