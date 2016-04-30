## Put comments here that give an overall description of what your
## function do
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
##                makeCacheMatrix above. If the inverse has already been calculated 
##                (and the matrix has not changed), then the cachesolve should retrieve 
##                the inverse from the cache.

## Write a short comment describing this function
## The function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## assumes x is always a invertible matrix
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  data = x$get()
  inv = solve(data, ...)
  
  x$setinv(inv)
  
  inv
}
