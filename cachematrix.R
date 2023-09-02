## Put comments here that give an overall description of what your
## functions do

## This function provides the methods for storing the data for our original matrix and the data produced by the cacheSolve function after.

# Makes a Special matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # Defines the inverse matrix
  inv <- NULL
  
  # we can set the matrix we want by calling this method
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Saves the matrix to this object
  get <- function() x
  
  # Define the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Creates a variable to keep the last inverse matrix calculated, NULL in the instance
  getinverse <- function() inv
  
  # Returns a list of objects created
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse matrix and checks if was previously calculated

cacheSolve <- function(x, ...) {
  
  # Stores the value of inverse matrix in it
  inv <- x$getinverse()
  
  # Checks if the inv was previously calculated
  if(!is.null(inv)) {
    # Print the message and return the value if it was calculated before
    message("getting cached data")
    return(inv)
  }
  
  # If was not previously calculated assign the matrix from makeCacheMatrix to data
  data <- x$get()
  
  # Calculates the inverse matrix
  inv <- solve(data, ...)
  
  # Saves the inverse matrix in the 
  x$setinverse(inv)
  
  # Returns the inverse matrix
  inv
}
