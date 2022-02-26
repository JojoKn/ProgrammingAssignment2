## Writing a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" 
## object that can cache its inverse
## Result of this function is basically a list of four functions, 
## two to get/set the matrix and two to get/set the inverse.
## No need to set starting value for matrix, as it is specified in the function. Inverse starting value is set to NULL
## With each new matrix that is set, the value of the inverse is set to NULL to avoid retrieving incorrect inverses.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function () inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above
## Step 1 is to retrieve the inverse from the function above. If it exists, message is sent and inverse returned.
## Otherwise, the inverse is calculated, and stored using the setter function for the inverse defined above. Afterwards,
## the inverse is returned.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
