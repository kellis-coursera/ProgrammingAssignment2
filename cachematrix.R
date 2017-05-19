## In order to save computer cycles, we are storing the inverse of
## matrices so that, in the case they are calculated in multiple
## instances, the answer will be retrieved from storage instead
## of having to do a possible costly inversion calculation again.

## This fucntion create a cachable matrix object from a given
## matrix. In addition to storing the matrix, it also is capable
## of retaining the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(in_inverse) inverse <<- in_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function is responsible for calculating the inverse or our
## cached matrix object created above. It finds the inverse by
## first checking to see if it has already been calcuated and
## stored in the cached matrix object. If not, it retrieves the
## matrix from the object, calculated the inverse, stores it in the
## object, and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
