## These functions are used to support the calculation of the 
## inverse of a matrix.  Since calculating the inverse can be 
## costly, these funcitons allow the "cacheing" of the inverse
## and subsequent retrieval (assuming the matrix hasn't changed)
## of the matrix inverse.

## This function will create a "special" matrix object that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      ## Create and initialize the necessary variables
      inverse <- NULL
      
      ## The get function returns the original matrix
      get <- function() x
      
      ## The set function sets the original matrix value and
      ## ensures that the inverse is set to NULL (not yet
      ## calculated)
      set <- function (y) {
            x <<- y
            inverse <<- NULL
      }
      
      ## The getinverse function returns the calculated inverse
      ## of the original matrix
      getinverse <- function() inverse
      
      ## The setinverse function places the passed matrix 
      ## into the inverse variable
      setinverse <- function(y) {
            inverse <<- y
      }
      
      ## Create a list object of the *class* functions
      list(get = get, 
           set = set,
           getinverse = getinverse,
           setinverse = setinverse)
}


## This function will return the inverse of the provided 
## matrix.  It takes full advantage of the "cacheing"
## functionality (the inverse is returned rather than 
## computed if already calculated)
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if (!is.null(inverse)) {
            message("Getting cached data")
            return(inverse)
      }
      
      ## Get the data (original matrix) from the x object
      ## and calculate the inverse.  
      data <- x$get()
      inverse <- solve(data)
      
      ## save the inverse to the "cache" object
      x$setinverse(inverse)
      
      ## return the calculated inverse of the matrix
      inverse
}
