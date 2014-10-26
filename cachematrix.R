## Put comments here that give an overall description of what your
## functions do

## Matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
#Setting the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
#Getting the matrix
  get <- function() x
#Setting the inverse
  setinverse<- function(inverse)
    inv_x <<-inverse
#Getting the inverse
  getinverse <- function()
    inv_x
#Returning a list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



#Computing the inverse of the matrix from makeCacheMatrix. 
#If inverse already been computed, use the cached matrix.

cacheSolve <- function(x, ...) {
        # Returning the inverse matrix of 'x'
  inv_x <-x$getinverse()

  if(!is.null(inv_x)) {
  message("getting cached inverse matrix")
  return(inv_x)
  }
    # Calculate the inverse using matrix multiplication
    data <-x$get()
    inv_x <- solve(data, ...)
    # Set the inverse to the object
    x$setinverse(inv_x)
    # Return the matrix
    return(inv_x)

}
