## Peer Graded Assignment: Programming Assignment 2: Lexical Scoping 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) { 
  matrixinv <- NULL 
  set <- function(y) { 
    x <<- y 
    matrixinv <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) matrixinv <<- inverse 
  getinverse <- function() matrixinv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 




## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  matrixinv <- x$getinverse() 
  if(!is.null(matrixinv)) { 
    message("getting cached data.") 
    return(matrixinv) 
  } 
  data <- x$get() 
  matrixinv <- solve(data) 
  x$setinverse(matrixinv) 
  matrixinv 
} 
