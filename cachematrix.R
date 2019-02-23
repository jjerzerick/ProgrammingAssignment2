

## What "makeCacheMatrix" does is create a list of functions called "set", "get", "setinverse", and "getinverse." The "set" argument will 
## first assign the value of the initial matrix, while the "get" arguement will retrieve the value of the matrix. The "setinverse" and "getinverse"
## arguements will set and get the inverse of the initial matrix

makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL
      set <- function(y) {
            x <<- y
            mx <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) mx <<- solve
      getinverse <- function() mx
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## CacheSolve will first check if the inverse of matrix 'x' has been created, and if so, will retrieve the cached version of the matrix (mx).
## If the inverse hasn't been created, it will use the 'solve' function in order to calculate the inverse of the matrix and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mx <- x$getinverse()
      if(!is.null(mx)) {
            message("getting cached data")
            return(mx)
      }
      data <- x$get()
      mx <- solve(data, ...)
      x$setinverse(mx)
      mx
}

## See below for the tested version of the functions:
##
##> source("cachematrix.R")
##> x<- matrix(rnorm(16), nrow=4, ncol=4)
##> myMatrix<-makeCacheMatrix(x)
##> cacheSolve(myMatrix)
##[,1]      [,2]       [,3]        [,4]
##[1,]  0.8930520  2.239847 -1.5897517  0.05234154
##[2,] -0.6308484 -2.778830 -0.7410956  0.30507908
##[3,]  1.0500117 -2.186016 -4.2097018  0.58581394
##[4,] -1.3988022 -2.924562  1.8027080 -0.55409926
##> cacheSolve(myMatrix)
##getting cached data
##[,1]      [,2]       [,3]        [,4]
##[1,]  0.8930520  2.239847 -1.5897517  0.05234154
##[2,] -0.6308484 -2.778830 -0.7410956  0.30507908
##[3,]  1.0500117 -2.186016 -4.2097018  0.58581394
##[4,] -1.3988022 -2.924562  1.8027080 -0.55409926
