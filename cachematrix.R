## Creation of a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialization of the inverse matrix        
  x_inv <- NULL
  
  # Set the matrix  
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  
  # Get the matrix   
  get <- function() x
  
  # Set the inverse matrix          
  setinv <- function(solve) x_inv <<- solve
  
  # Get the inverse matrix 
  getinv <- function() x_inv
  
  # Coerce and check        
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)   
}

## Compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  x_inv <- x$getinv()
  
  # Check if inverse matrix already exists and return it
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  
  # Calculate and return inverse matrix when it doesn't exist
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}

