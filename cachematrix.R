# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


# Function that creates a special "matrix", which is a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The function calculates the inversion of the special "matrix" created with the above function. , 
# First checks to see if the inversion has already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inversion of the matrix and sets it in the cache via the setmean function.

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
  
}