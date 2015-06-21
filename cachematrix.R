## Assignment 2
## R Programming 2015
## Brendan Eck


## This generates the cached version (environment) of the matrix "X".

makeCacheMatrix <- function(X = numeric()) {
  
  # Below we set the four fields which make up the "CacheMatrix" data structure
  # Null out the inverse (we do not yet have it)
  m <- NULL
  
  # Set the value of the matrix. Only use this if you want to
  # change the matrix "X" to matrix "Y". Not sure why this is useful....
  # May be useful in some other "CacheFunctions" like CacheSolve.R which use
  # this environment.
  set <- function(Y) {
    X <<- Y
    m <<- NULL
  }
  
  # Get the original matrix, return it to workspace
  get <- function() X
  
  # Assign the inverted matrix to the data structure
  setinv <- function(inv) m <<- inv
  
  # Get the inverted matrix, return it to workspace
  getinv <- function() m
  
  # Output the allocated "CacheMatrix"
  output <- list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
  
  output 
}

## Computes or accesses cached inverse of the cache matrix "X".

cacheSolve <- function(X, ...) {
  # First, check to see if the input "CacheMatrix" already has had its inverse
  # computed
  m <- X$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If getinv(X) returned "NULL", then we need to compute the matrix inversion
  message("No cached data found. Performing matrix inversion..")
  data <- X$get()
  m <- solve(data, ...)
  X$setinv(m)
  m
}
