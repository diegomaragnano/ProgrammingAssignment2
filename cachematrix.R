# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Setter function to set the matrix value
  set <- function(newValue) {
    x <<- newValue
    inv <<- NULL
  }
  
  # Getter function to retrieve the matrix value
  get <- function() {
    x
  }
  
  # Setter function to set the inverse value
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Getter function to retrieve the inverse value
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(m, ...) {
  # Check if the inverse is already cached
  if (!is.null(m$getInverse())) {
    message("Getting cached inverse")
    return(m$getInverse())
  }
  
  # Calculate the inverse
  inverse <- solve(m$get(), ...)
  
  # Cache the inverse
  m$setInverse(inverse)
  
  inverse
}

# How to test it in the console
# # Create a special matrix object
# mat <- makeCacheMatrix(matrix(c(4, 3, 2, 1), nrow = 2))
# 
# # Get the matrix
# mat$get()
# 
# # Compute and cache the inverse
# cacheSolve(mat)
# 
# # Get the cached inverse
# mat$getInverse()


