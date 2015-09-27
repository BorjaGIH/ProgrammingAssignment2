makeCacheMatrix<- function(x=matrix()){
  	##This function can create a "matrix-like" object, modify it and cache it's inverse. 
  
  	## Display info message
  	message("Function for the special matrix object. Use objectname$set() and objectname$get() to 
	respectively set and retrieve the matrix.")
  
 	m <- NULL
  
  	##"Subfunction" for setting the matrix. Value is assigned in the cache (different environment
  	## than the current one)
  	set <- function(y=matrix()) {
    		x <<- y
    		m <<- NULL
  	}
  
  	##"Subfunction" for getting the matrix
  	get <- function() x
  
  	##"Subfunction" for setting the inverse. Value is setted in the cache (different environment 
  	## than the current one)
  	setInverse <- function(inverse) m <<- inverse
  
  	##"Subfunction" for getting the inverse
  	getInverse <- function() m
  
  	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve<- function(x,...){
  	## This function handles the "matrix-like" object created with makeCacheMatrix() function. It
  	## checks if the inverse is stored in the cache, and, if not, it calculates it and stores there.
  
  	## Check for the inverse stored in the cache from makeCacheMatrix() function
  	m <- x$getInverse()
  
  	## If there is such a thing, retrieve it and skip the calculation with the "return" command
  	if(!is.null(m)) {
    		message("Getting cached inverse")
    		return(m)
  	}
  
  	## Otherwise, get the matrix...
  	data <- x$get()
  
  	## calculate the inverse...
  	m <- solve(data, ...)
  
  	## store it in the cache...
  	x$setInverse(m)
  
  	## and return print it.
  	m
}
