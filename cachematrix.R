## Functions to create a cached version of a matrix and its inverse

## makeCacheMatrix - create a cached matrix with functions for seting, retrieving,
## setting the inverse, and getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
     
     # Declare an empty variable to hold the inverse
     i <- NULL
     
     # Declare a set function to hold the matrix "x"
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     # Declare a get function to retrieve the cached matrix
     get <- function() x
     
     # Declare a function to set the value of m to the inverse of the cached matrix
     setinverse <- function(solve) i <<- solve
     
     # Return the value of the cached inverse (i.e., "m")
     getinverse <- function() i
     
     # A list of the functions contained in this function
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## A function that checks to see if the inverse of the supplied cached matrix has been calculated
## If yes, return the cached value, if not, calculate the new value
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     # First, use getinverse() to see if the value is already stored
     i <- x$getinverse()
     
     # If the value of the inverse is not NULL, then let us know we're using the cached value
     # and return the cached value
     if(!is.null(i)) {
          message("Getting cached inverse of supplied matrix.")
          return(i)
     }
     
     # We only get here if there was not a cached value
     # Set the value of a local variable, data, to the global value of the matrix
     # stored in "x"
     data <- x$get()
     
     # Calculate the inverse using the built in R function - "solve"
     i <- solve(data)
     
     # Use the global defined function "setinverse" to save the matrix's inverse value as m
     x$setinverse(i)
     
     # Return the matrix inverse value
     i
}
