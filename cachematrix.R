## This functions can be used to calculate the inverse of a matrix and to save 
## the calculated value in a cache.
## The cache is created with the makeCacheMatrix function
## The cache is used when the cacheSolve function is called a second time with 
## a identical matrix

##
## makeCacheMatrix
##
## Parameters
## x     square matrix with integer values
##
## Creates a list with 4 functions.
## Function set: saves the matrix x and clear the content of the saved inverse value
## Function get: returns the saved matrix x
## Function setInverse: generate en save the inverse of the saves matrix x
## Function getInverse: returns the saved inverse matrix

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y) {
                    x <<- y
                    i <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) i <<- inverse
            getInverse <- function() i
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}

##
## cacheSolve
## Function to 
##   - save the matrix "x" in the cache "cache" when the cache did not contain an identical matix
##   - return then cached inverse for the matrix "x" when the matrix is identical with the cached matrix
##   - compute and cache the inverse of the matrix "x" when the matrix is not identical with the cached matix
##
## Parameters 
## x     Square matrix
## 
## Returns inverse of x 
## 

cacheSolve <- function(x, ...) {
   ##
   ## Test the current value of the cache
   ## and save new matrix if not identical
   ##
   if ( !identical(x,cache$get() ) ){
      message ("cache matrix")
      cache <<- makeCacheMatrix(x)
      data <- cache$get()
      i <- solve(data)
      cache$setInverse(i)
   }
   ## Return a matrix that is the inverse of 'x'
   i <- cache$getInverse()
   if(!is.null(i)) {
      message("getting cached data")
   }
   i
}
