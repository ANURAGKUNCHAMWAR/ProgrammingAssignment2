## Below function calculates the inverse of a matrix and saves in the cache
## so that the next time if user wants to find the inverse he might not need to
## calculate again

## Below code takes care to set the value of given matrix , get / find the value 
## matrix and then the same for inverse of the matrix too. 

makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL ## define the cache z
  set <- function(y) {
    x <<- y ## assigning input matrix to the parent environment variable
    z <<- NULL ## reinitializing in the parent environment
  }
  get <- function() x ## returns the matrix x
  setinverse <- function(inverse) z <<- inverse ## sets the cache z to the inverse of matrix 
  getinverse <- function() z ## returns the cached inverse of matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## The below function calculates the inverse of the matrix returned from the 
## above function , before it procceeds to calculate the inverse of the matrix,
## it first checks if inverse exists and if so will retrieve the value. 

cacheSolve <- function(x, ...) {   ## returns a matrix that is inverse of matrix x
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
