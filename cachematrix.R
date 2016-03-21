## Put comments here that give an overall description of what your
## functions do
##
## Matrix inversion is usually a costly computation and there are always
## benefits to speeding up load times/caching with computation.  Here are
## two ways ways of caching the inverse of a matrix rather than computing it
## repeatedly.
##
## Write a short comment describing this function: makeCacheMatrix()
##
## Similar to how the example 'makeVector' created a special "vector",
## the function makeCacheMatrix() creates a list with a special function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse


makeCacheMatrix <-function(x = matrix()) {
  inv <-NULL
  set <-function(y) {
      x <<-y
      inv <<-NULL
  }
  get <-function() x
  setInverse <-function(inverse) inv <<-inverse
  getInverse <-function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

##
##

## Write a short comment describing this function
## The function cacheSolve() returns the inverse of a matrix.
## It also first checks to see if the inverse was recently computed and
## gives that result, in order to save computation time for the user.  
## Otherwise, the function will computer the square matrix's inverse and
## then use the setInverse() from before to set it's value in cache.

cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
    inv <-x$getInverse()
    if(!is.null(inv)) {
        message("...data in cache...")
        return(inv)
    }
    data <-x$get()
    inv <-solve(data)
    x$setInverse(inv)
    inv
}






















