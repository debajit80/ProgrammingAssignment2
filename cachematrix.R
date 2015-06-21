## Put comments here that give an overall description of what your
## functions do
## The functions store the inverse of the matrix as passed through the main function.
## If the matrix does not change, the inverse is retrieved from the cache saving processing time.
## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL 
              ## set the matrix
                set <- function(y){
                      y <<- x   ##substitutes x from the main function in y
                      i <<- NULL ##restores to null the value of inverse i              
                                    }
              ## get the matrix
              get <- function() x
              ##set the inverse matrix
              setinverse <- function(solve) i <<- solve ##store the value of the input in a variable i into the main function
              ##get the inverse matrix
              getinverse <- function()i 
              
                  list(set=set, get=get,
                        setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the matrix has not changed, then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

              i <- x$getinverse() ##The cacheSolve checks the matrix i stored previously exists and is not NULL              
              if(!is.null(i)) {
                              message("getting cached data")
                                      return(i) ##If the matrix i is not NUll, then its returns the matrix i from the cache
                                          }
                          data <- x$get()       ##gets the matrix stored with the makeCache matrix function
                          
                          i <- solve(data, ...) ##takes the inverse of the matrix
                          x$setinverse(i)       ##stores the inverse matrix 
                          i                     ##returns the inverse matrix
                                                }






