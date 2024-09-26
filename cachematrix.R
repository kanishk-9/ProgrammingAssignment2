#  The makeCacheMatrix function creates a special "matrix" object that can cache (store) its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL  # Initializing the inverse as NULL
  
  set <- function(y) {
    x <<- y  # Set value of matrix
    inv_m <<- NULL  # Resetting the cached inverse when the matrix changes
  }
  
  get <- function() x  # Get value of the matrix
  
  setInverse <- function(inverse) inv_m <<- inverse  # Set the cached inverse
  
  getInverse <- function() inv_m  # Get cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv_m <- x$getInverse()  # Check if the inverse is already cached
  
  if(!is.null(inv_m)) {
    message("getting cached inverse")
    return(inv_m)  # Return the cached inverse (if available)
  }
  
  # Compute the inverse if not cached
  data <- x$get() 
  inv_m <- solve(data, ...)  # Compute the inverse using the `solve` function
  
  x$setInverse(inv_m)
  inv_m  # Return the computed inverse
        
}

# Example:
# Create a matrix
m1 <- matrix(c(4, 0, 3, 1), nrow = 2, ncol = 2, byrow = TRUE)
m1

# Create a special "matrix" object
cache_matrix <- makeCacheMatrix(m1)

# Compute the inverse (this will calculate and cache it)
inverse_matrix <- cacheSolve(cache_matrix)
print(inverse_matrix)

# This should fetch the cached inverse
cached_inverse <- cacheSolve(cache_matrix)
print(cached_inverse)
