## Programming Assignment 2: Lexical Scoping 
## Feb 2017
## Author: Akhilesh Sharma
## 
## Creating a Special "Matrix" object and computing the Inverse of that "Matrix" object
## Assumption: The matrix supplied is always invertible. 
## Test Cases: I have tested this with square matrices of 2x2 and 3x3
## Examples used: 
## 1. matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## 2. matrix(c(4, 2, 2, 2, 3, 1, 2, 1, 3), nrow = 3, ncol = 3)


## Function 1 - This creates the matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              set <- function(y) {
                      x <<- y
                      i <<- NULL
              }
              get <- function() x
              setinv <- function(solve) i <<- solve
              getinv <- function() i
              list(set = set, get = get, 
                  setinv = setinv,
                  getinv = getinv)
}


## Function 2 - This computes the inverse of the matrix returned by Function 1. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinv()
          if(!is.null(i)){
              message("getting cached data")
              return(i)
          }
          matin <- x$get()
          i <- solve(matin, ...)
          x$setinv(i)
          i
}
