## The first function makes the matrix and the second function solves it


##Computing the inverse of a square matrix can be done with the solve function in R. 
##Eg. if X is a square invertible matrix, then solve(X) returns its inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() { #returns x
      x
    }
    setinv <- function(solve) { ##searches for m in global env
      m <<- solve
    }
    getinv <- function() { ##returns m (either global or local)
      m
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
