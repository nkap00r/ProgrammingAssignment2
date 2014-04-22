## The functions allow to create a special matrix object and cache inverse 
## of a square matrix (for which inverse can be calculated)


## This function create a special matrix object and exposes a set of functions
## that allow to store & retrieve the cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function first checks if inverse is stored in cache for the given matrix
## object and returns is available (i.e. not null). If not found (ie. null)
## it calculates the inverse and sets it in cache by calling setinverse function
## defined above and finally returns the inverse. In case the matrix is not 
## invertible it throws an error.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
