## The first function makes the matrix and the second function solves it


##Computing the inverse of a square matrix can be done with the solve function in R. 
##Eg. if X is a square invertible matrix, then solve(X) returns its inverse.
## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL #to clear cache
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
    #returns list with the 4 functions defined above
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of matrix x

cacheSolve <- function(x, ...) {
  m <- x$getinv() #gets the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Calculates inverse if cache is empty
  data <- x$get() #gets new matrix
  m <- solve(data, ...) #calulates inverse
  x$setinv(m) #sets or caches inverse
  m
}
