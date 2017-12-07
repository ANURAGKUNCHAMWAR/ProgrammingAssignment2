## Below function calculates the inverse of a matrix and saves in the cache
## so that the next time if user wants to find the inverse he might not need to
## calculate again

## Below code takes care to set the value of given matrix , get / find the value 
## matrix and then the same for inverse of the matrix too. 

makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL
  set <- function(y) {
    x <<- y 
    z <<- NULL 
  }
  get <- function() x 
  setinverse <- function(inverse) z <<- inverse 
  getinverse <- function() z 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## The below function calculates the inverse of the matrix returned from the 
## above function , before it procceeds to calculate the inverse of the matrix,
## it first checks if inverse exists and if so will retrieve the value. 

cacheSolve <- function(x, ...) {
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
